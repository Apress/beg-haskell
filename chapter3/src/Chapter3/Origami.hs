{-# LANGUAGE LambdaCase #-}

module Chapter3.Origami where

import Data.List

filterAsFold :: (a -> Bool) -> [a] -> [a]
filterAsFold p = foldr (\x l -> if p x then x : l else l) []

mapAsFold :: (a -> b) -> [a] -> [b]
mapAsFold f = foldr (\x l -> f x : l) []

enumUnfold :: Int -> Int -> [Int]
enumUnfold n m = unfoldr (\x -> if x > m then Nothing else Just (x, x+1)) n

minSort :: Ord a => [a] -> [a]
minSort = unfoldr (\case [] -> Nothing
                         xs -> Just (m, delete m xs) where m = minimum xs)

foldr2 :: (Maybe (a,b) -> b) -> [a] -> b
foldr2 f []     = f Nothing
foldr2 f (x:xs) = f $ Just (x, foldr2 f xs)

mapAsFold2 :: (a -> b) -> [a] -> [b]
mapAsFold2 f = foldr2 (\case Nothing     -> []
                             Just (x,xs) -> f x : xs)