{-# LANGUAGE BangPatterns #-}

module Chapter5.Problems where

import Chapter5.Infinite

sumForce :: [Integer] -> Integer
sumForce xs = sumForce' xs 0
              where sumForce' []     z = z
                    sumForce' (y:ys) z = sumForce' ys $! (z+y)

sumYears :: [TimeMachine] -> Integer
sumYears xs = sumYears' xs 0
              where sumYears' []            z = z
                    sumYears' (TM _ !y :ys) z = let !s = z + y in sumYears' ys s