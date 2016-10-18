{-# LANGUAGE TemplateHaskell #-}

module Chapter6.KMeansLens (kMeans) where

import Chapter6.Vector

import Control.Lens

import Data.List
import qualified Data.Map as M

data KMeansState e v = KMeansState { _centroids :: [v], _points :: [e]
                                   , _err :: Double, _threshold :: Double
                                   , _steps :: Int }

makeLenses ''KMeansState

initializeState :: (Int -> [e] -> [v]) -> Int -> [e] -> Double -> KMeansState e v
initializeState i n pts t = KMeansState (i n pts) pts (1.0/0.0) t 0

clusterAssignments :: (Vector v, Vectorizable e v) => KMeansState e v -> M.Map v [e]
clusterAssignments state =
  let initialMap = M.fromList $ zip (state^.centroids) (repeat [])
   in foldr (\p m -> let chosenCentroid = minimumBy (\x y -> compare (distance x $ toVector p)
                                                                     (distance y $ toVector p))
                                                    (state^.centroids)
                      in M.adjust (p:) chosenCentroid m)
            initialMap (state^.points)
   

kMeans :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> [v]
kMeans i n pts t = view centroids $ kMeans' (initializeState i n pts t)

kMeans' :: (Vector v, Vectorizable e v) => KMeansState e v -> KMeansState e v
kMeans' state = let assignments = clusterAssignments state
                    state1      = state  & centroids.traversed
                                         %~ (\c -> centroid $ fmap toVector $ M.findWithDefault [] c assignments)
                    state2      = state1 & err              .~ sum (zipWith distance (state^.centroids) (state1^.centroids))
                    state3      = state2 & steps            +~ 1
                 in if state3^.err < state3^.threshold then state3 else kMeans' state3
