module Chapter7.MonadPlus where

import Control.Monad

broken1 :: Integer -> [Integer]
broken1 n = [n-1, n+1]

broken2 :: Integer -> [Integer]
broken2 n = [1024, n+2]

find_ :: (a -> Bool) -> [a] -> Maybe a
find_ f = msum . map (\x -> if f x then Just x else Nothing)