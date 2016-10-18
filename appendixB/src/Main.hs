{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Monad.Tardis

main :: IO ()
main = print $ sumListTardis [1,2,3,4]

sumListTardis :: [Int] -> [(Int,Int)]
sumListTardis lst = evalTardis (sumListTardis' lst) (0, 0)

sumListTardis' :: [Int] -> Tardis Int Int [(Int,Int)]
sumListTardis' (x:xs) = do
  sumFw <- getPast
  let newFw = sumFw + x
  sendFuture $ newFw
  rec let newBw = sumBw + x
      sendPast $ newBw
      sumBw <- getFuture
  rest <- sumListTardis' xs
  return $ (newFw, newBw):rest
sumListTardis' [] = return []
