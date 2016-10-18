{-# LANGUAGE FlexibleContexts #-}

module Chapter6.KMeansRWS (kMeans) where

import Chapter6.Vector

import Control.Monad.RWS
-- import Control.Monad.RWS.Class

import Data.List
import qualified Data.Map as M

 
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

kMeans' :: (Vector v, Vectorizable e v) => [e] -> RWS Double (Sum Int) [v] ()
kMeans' points = do prevCentrs  <- get
                    let assignments = clusterAssignments prevCentrs points
                        newCentrs = newCentroids assignments
                    put newCentrs
                    tell (Sum 1)
                    t <- ask
                    let err = sum $ zipWith distance prevCentrs newCentrs
                    unless (err < t) $ kMeans' points

kMeans :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> ([v], Sum Int)
kMeans i n pts t = execRWS (kMeans' pts) t (i n pts)

newtype MyWriter m a = MyWriter (a,m)

instance Monoid m => Monad (MyWriter m) where
  return x = MyWriter (x,mempty)
  (MyWriter (a,x)) >>= f = let MyWriter (b,y) = f a
                            in MyWriter (b,x `mappend` y)
