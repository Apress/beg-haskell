module Main where

import Test.Hspec
import Test.HUnit

import Data.Maybe

import Chapter15.BinaryTree

main :: IO()
main = hspec $ do
  describe "Insertion in binary tree" $ do
    it "Inserts correctly 1 in empty tree" $
      treeInsert 1 Leaf @?= Node 1 Leaf Leaf
    it "Finds 1 after inserting it on a tree" $
      isJust $ treeFind 1 $ treeInsert 1 (Node 2 Leaf Leaf)
    it "Gets the minimum correctly" $
      pendingWith "Needs to be implemented"
      
