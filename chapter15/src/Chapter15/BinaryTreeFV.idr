module Chapter15.BinaryTreeFV

data BinaryTree : Type -> Nat -> Type where
  Leaf : BinaryTree a 0
  Node : a -> BinaryTree a l -> BinaryTree a r -> BinaryTree a (S (l + r))

treeInsert : Ord a => a -> BinaryTree a n -> BinaryTree a (S n)
treeInsert x Leaf = Node x Leaf Leaf
treeInsert x (Node y l r) with (x <= y)
  | True   = let l' = treeInsert x l in Node y l' r
  | False ?= let r' = treeInsert x r in Node y l r'

-- Do what has been done in Idris interactive mode
Chapter15.BinaryTreeFV.treeInsert_lemma_1 = proof
  intros
  rewrite sym (plusSuccRightSucc l r)
  trivial

treeMerge : Ord a => BinaryTree a n -> BinaryTree a m -> BinaryTree a (n + m)
treeMerge t1 Leaf ?= t1
treeMerge t1 (Node x2 l2 r2) ?= treeInsert x2 (treeMerge (treeMerge t1 l2) r2)

Chapter15.BinaryTreeFV.treeMerge_lemma_1 = proof
  intros
  rewrite sym (plusZeroRightNeutral n)
  trivial

{-
Chapter15.BinaryTreeFV.treeMerge_lemma_2 = proof
  intros
  rewrite sym (plusCommutative n (S (plus l r)))
  rewrite sym (plusCommutative (plus l r) n)
  rewrite sym (plusAssociative n l r)
  trivial
-}

Chapter15.BinaryTreeFV.treeMerge_lemma_2 = proof
  intros
  rewrite (plusSuccRightSucc n (plus l r))
  rewrite  sym (plusAssociative n l r)
  trivial

{-
using (x : a, y : a, l : BinaryTree a ln, r : BinaryTree a rn)
  data In : a -> BinaryTree a n -> Type where
    AtNode  : Ord a => In x (Node x l r)
    AtLeft  : Ord a => In x l -> In x (Node y l r)
    AtRight : Ord a => In x r -> In x (Node y l r)

insertIsThere : Ord a => (x : a) -> (t : BinaryTree a n) -> In x (treeInsert x t)
insertIsThere x Leaf = AtNode
insertIsThere x (Node y l r) with (x <= y, treeInsert x (Node y l r))
  | (True, Node _ l' _) = AtLeft (insertIsThere x l )
  | (False,Node _ _ r') = ?e2 -- $ insertIsThere x r
-}
