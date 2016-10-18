-- | Simple implementation of binary trees
module Chapter15.BinaryTree (
  -- * The main data type
  BinaryTree(..),
  -- * Operations
  -- ** Insertion
  treeInsert,
  treeMerge,
  -- ** Lookup
  treeFind,
  treeFindMin,
  -- ** Removal
  treeDelete
) where

import Test.QuickCheck

-- | A typical binary tree
data BinaryTree a = Node a (BinaryTree a) (BinaryTree a) -- ^Inner nodes
                  | Leaf                                 -- ^Leaves
                  deriving (Eq, Show)

{-|
Inserts an element into a 'BinaryTree'

 * If it finds a leaf, insert there
 
 * If smaller than the item in the node, insert in the left
 
 * If larger than the item in the node, insert in the right

>>> treeInsert 1 Leaf
Node 1 Leaf Leaf

prop> size (treeInsert x t) = size t + 1
-}
treeInsert :: Ord a => a -> BinaryTree a -> BinaryTree a
treeInsert x Leaf = Node x Leaf Leaf
treeInsert x (Node y l r) | (x <= y)  = Node y (treeInsert x l) r
                          | otherwise = Node y l (treeInsert x r)

treeFindMin :: BinaryTree a -> Maybe a
treeFindMin Leaf = Nothing
treeFindMin (Node x Leaf _) = Just x
treeFindMin (Node _ l    _) = treeFindMin l

treeFind :: Ord a => a -> BinaryTree a -> Maybe a
treeFind _ Leaf = Nothing
treeFind x (Node y l r) | x < y  = treeFind x l
                        | x > y  = treeFind x r
                        | otherwise = Just y

{-|
Merges two trees by repeated insertion

prop> treeMerge Leaf Leaf = Leaf
-}
treeMerge :: Ord a => BinaryTree a -> BinaryTree a -> BinaryTree a
treeMerge t Leaf         = t
treeMerge t (Node x l r) = treeInsert x $ treeMerge (treeMerge t l) r

treeDelete :: Ord a => a -> BinaryTree a -> BinaryTree a
treeDelete _ Leaf = Leaf
treeDelete x (Node y l r)
  | x < y = Node y (treeDelete x l) r
  | x > y = Node y l (treeDelete x r)
  | otherwise = case (l, r) of
    (Leaf, Leaf) -> Leaf
    (_,    Leaf) -> l
    (Leaf, _   ) -> r
    (_,    _   ) -> Node y l r' where
                      Just m = treeFindMin r
                      r' = treeDelete m r

instance Arbitrary a => Arbitrary (BinaryTree a) where
  arbitrary = sized $ \n ->
    if (n == 0)
       then return Leaf
       else frequency [(1, return Leaf),
                       (n, resize (n-1) arbitrary)]
  shrink Leaf         = []
  shrink (Node _ l r) = [l, r]

