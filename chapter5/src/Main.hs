module Main where

import Data.List

main :: IO ()
main = putStrLn $ show result

result :: Integer
result = foldl' (*) 1 [1 .. 100000]