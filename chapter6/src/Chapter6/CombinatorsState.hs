{-# LANGUAGE TypeSynonymInstances, LiberalTypeSynonyms #-}

module Chapter6.CombinatorsState where

import Chapter6.Vector

import Data.Default
import Data.List
import qualified Data.Map as M

type State s a = s -> (a, s)

thenDo :: State s a -> (a -> State s b) -> State s b
-- thenDo :: (s -> (a,s)) -> (a -> s -> (b,s)) -> s -> (b,s)
--thenDo f g s = let (resultOfF, stateAfterF) = f s
--                in g resultOfF stateAfterF
thenDo f g = uncurry g . f

data KMeansState v = KMeansState { centroids :: [v]
                                 , threshold :: Double
                                 , steps :: Int }

newCentroids :: (Vector v, Vectorizable e v) => M.Map v [e] -> [v]
newCentroids = M.elems . fmap (centroid . map toVector)

clusterAssignments :: (Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignments centrs points =
  let initialMap = M.fromList $ zip centrs (repeat [])
   in foldr (\p m -> let chosenCentroid = minimumBy (\x y -> compare (distance x $ toVector p)
                                                                     (distance y $ toVector p))
                                                    centrs
                      in M.adjust (p:) chosenCentroid m)
            initialMap points
            
kMeans' :: (Vector v, Vectorizable e v) => [e] -> State (KMeansState v) [v]
kMeans' points =
  (\s -> (centroids s,s))                                  `thenDo` (\prevCentrs  ->
  (\s -> (clusterAssignments prevCentrs points, s))        `thenDo` (\assignments ->
  (\s -> (newCentroids assignments, s))                    `thenDo` (\newCentrs   ->
  (\s -> ((), s { centroids = newCentrs }))                `thenDo` (\_           ->
  (\s -> ((), s { steps = steps s + 1 }))                  `thenDo` (\_           ->
  (\s -> (threshold s, s))                                 `thenDo` (\t           ->
  (\s -> (sum $ zipWith distance prevCentrs newCentrs, s)) `thenDo` (\err  ->
  if err < t then (\s -> (newCentrs, s)) else (kMeans' points) )))))))

initialState :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> KMeansState v
initialState i k pts t = KMeansState (i k pts) t 0

kMeans :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> [v]
kMeans i k pts t = fst $ kMeans' pts (initialState i k pts t)

remain :: a -> (s -> (a,s))
remain x = \s -> (x,s)

access :: (s -> a) -> (s -> (a,s))
access f = \s -> (f s, s)

modify :: (s -> s) -> (s -> ((), s))
modify f = \s -> ((), f s)

kMeans2 :: (Vector v, Vectorizable e v) => [e] -> State (KMeansState v) [v]
kMeans2 points =
  access centroids                                     `thenDo` (\prevCentrs  ->
  remain (clusterAssignments prevCentrs points)        `thenDo` (\assignments ->
  remain (newCentroids assignments)                    `thenDo` (\newCentrs   ->
  modify (\s -> s { centroids = newCentrs })           `thenDo` (\_           ->
  modify (\s -> s { steps = steps s + 1 })             `thenDo` (\_           ->
  access threshold                                     `thenDo` (\t           ->
  remain (sum $ zipWith distance prevCentrs newCentrs) `thenDo` (\err         ->
  if err < t then remain newCentrs else kMeans2 points )))))))
