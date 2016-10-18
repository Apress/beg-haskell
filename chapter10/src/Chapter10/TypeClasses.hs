{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Chapter10.TypeClasses where

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Data.Traversable

data BinaryTree2 a = Node2 a (BinaryTree2 a) (BinaryTree2 a)
                   | Leaf2
                   deriving (Show, Functor, Foldable, Traversable)

{-
instance Functor BinaryTree2 where
  fmap f (Node2 x l r) = Node2 (f x) (fmap f l) (fmap f r)
  fmap _ Leaf2         = Leaf2

instance Foldable BinaryTree2 where
  foldMap f (Node2 x l r) = (f x) <> (foldMap f l) <> (foldMap f r)
  foldMap _ Leaf2         = mempty

instance Traversable BinaryTree2 where
  traverse f (Node2 x l r) = Node2 <$> f x <*> traverse f l <*> traverse f r
  traverse _ Leaf2         = pure Leaf2
-}
