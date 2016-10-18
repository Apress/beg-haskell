{-# LANGUAGE TemplateHaskell #-}

module Chapter6.KMeansStateLens (kMeans, initializeSimple) where

import Chapter6.Vector

import Control.Monad.State

import Control.Lens

import Data.List
import qualified Data.Map as M

data KMeansState v = KMeansState { _centroids :: [v]
                                 , _threshold :: Double
                                 , _steps :: Int }

makeLenses ''KMeansState

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
kMeans' points = do prevCentrs  <- use centroids
                    let assignments = clusterAssignments prevCentrs points
                        newCentrs = newCentroids assignments
                    centroids .= newCentrs
                    steps     += 1
                    let err = sum $ zipWith distance prevCentrs newCentrs
                    t <- use threshold
                    if err < t then return newCentrs else kMeans' points

kMeans :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> [v]
kMeans i n pts t = evalState (kMeans' pts) (initializeState i n pts t)

initializeSimple :: Int -> [e] -> [(Double,Double)]
initializeSimple 0 _ = []
initializeSimple n v = (fromIntegral n, fromIntegral n) : initializeSimple (n-1) v
