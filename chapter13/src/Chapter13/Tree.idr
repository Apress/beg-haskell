module Chapter13.Tree

data Tree : Nat -> Type -> Type where
  Leaf : a -> Tree 1 a
  Node : a -> Tree n a -> Tree m a -> Tree (1 + maximum n m) a

t : Tree 3 Char
t = Node 'a' (Leaf 'b') (Node 'c' (Leaf 'd') (Leaf 'e'))

{-
rotate : Tree n a -> Tree n a
rotate (Leaf x)     = Leaf x
rotate (Node x r l) = Node x (rotate l) (rotate r)
-}

