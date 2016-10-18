module Chapter6.KMeansState (kMeans) where

import Chapter6.Vector

import Control.Monad.State

import Data.List
import qualified Data.Map as M

data KMeansState v = KMeansState { centroids :: [v]
                                 , threshold :: Double
                                 , steps :: Int }

initializeState :: (Int -> [e] -> [v]) -> Int -> [e] -> Double -> KMeansState v
initializeState i n pts t = KMeansState (i n pts) t 0

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
kMeans' points = do prevCentrs <- fmap centroids get
                    let assignments = clusterAssignments prevCentrs points
                        newCentrs   = newCentroids assignments
                    modify (\s -> s { centroids = newCentrs })
                    modify (\s -> s { steps = steps s + 1 })
                    t <- fmap threshold get
                    let err = sum $ zipWith distance prevCentrs newCentrs
                    if err < t then return newCentrs else kMeans' points

kMeans :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> [v]
kMeans i n pts t = evalState (kMeans' pts) (initializeState i n pts t)
