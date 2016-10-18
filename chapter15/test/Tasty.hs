{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit as HU
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC

import Data.Maybe

import Chapter15.BinaryTree

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "Tasty Tests" [
    testGroup "HUnit Tests" [ 
      hunitTestInsertOnLeaf
    , hunitTestInsertFind 'b' Leaf
    , hunitTestInsertFind 'c' (Node 'd' Leaf Leaf)
    ]
  , reverseTests
  , quickCheckTests 
  ]

hunitTestInsertOnLeaf :: TestTree
hunitTestInsertOnLeaf = HU.testCase "Insert 'a' on empty tree" $
  HU.assertEqual "Insertion is wrong" (treeInsert 'a' Leaf) (Node 'a' Leaf Leaf)

hunitTestInsertOnLeaf' = HU.testCase "Insert 'a' on empty tree" $
  treeInsert 'a' Leaf HU.@?= Node 'a' Leaf Leaf

hunitTestInsertFind :: Ord a => a -> BinaryTree a -> TestTree
hunitTestInsertFind e t = HU.testCase "Insert can be found" $
  assertBool "Cannot find element" (isJust $ treeFind e $ treeInsert e t)

hunitTestInsertFind' :: Ord a => a -> BinaryTree a -> TestTree
hunitTestInsertFind' e t = HU.testCase "Insert can be found" $
  (isJust $ treeFind e $ treeInsert e t) HU.@? "Cannot find element"
  
reverseTests :: TestTree
reverseTests = testGroup "Tests over reverse"
  [ QC.testProperty "reverse respects length" $
      \(lst :: [Integer]) -> length (reverse' lst) == length lst ]

quickCheckTests :: TestTree
quickCheckTests = testGroup "QuickCheck Tests"
  [ QC.testProperty "insert => you will find it" $
      \(n :: Int) t -> treeFind n (treeInsert n t) == Just n
  , QC.testProperty "delete => not find it" $
      \(n :: Int) t -> (treeFind n t == Nothing) QC.==> (treeFind n $ treeDelete n $ treeInsert n t) == Nothing ]

reverse' :: [a] -> [a]
reverse' []     = []
reverse' [x]    = [x, x]
reverse' (x:xs) = reverse' xs ++ [x]
