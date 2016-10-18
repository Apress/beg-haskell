{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances, UndecidableInstances, GADTs, FlexibleContexts #-}

module Chapter13.CheckPresentsFnDeps where

data Zero
data Succ n

class Plus x y z | x y -> z
instance Plus Zero x x
instance Plus x y z => Plus (Succ x) y (Succ z)

class Min x y z | x y -> z
instance Min Zero     y    Zero
instance Min (Succ x) Zero Zero
instance Min x y z => Min (Succ x) (Succ y) (Succ z)

class Max x y z | x y -> z
instance Max Zero     y    y
instance Max (Succ x) Zero (Succ x)
instance Max x y z => Max (Succ x) (Succ y) (Succ z)

data Expr a r where
  AmountOf            :: a -> Expr a Integer
  PriceOf             :: a -> Expr a Float
  TotalNumberProducts :: Expr a Integer
  TotalPrice          :: Expr a Float
  Val                 :: Num n => n -> Expr a n
  (:+:)               :: Num n => Expr a n -> Expr a n -> Expr a n
  (:*:)               :: Num n => Expr a n -> Expr a n -> Expr a n
  (:<:)               :: Num n => Expr a n -> Expr a n -> Expr a Bool
  (:<=:)              :: Num n => Expr a n -> Expr a n -> Expr a Bool
  (:>:)               :: Num n => Expr a n -> Expr a n -> Expr a Bool
  (:>=:)              :: Num n => Expr a n -> Expr a n -> Expr a Bool
  (:&&:)              :: Expr a Bool -> Expr a Bool -> Expr a Bool
  (:||:)              :: Expr a Bool -> Expr a Bool -> Expr a Bool
  Not                 :: Expr a Bool -> Expr a Bool

data Vect n a where
  VNil  :: Vect Zero a
  VCons :: a -> Vect n a -> Vect (Succ n) a

data Offer a p where
  Present          :: a -> Offer a (Succ Zero)
  PercentDiscount  :: Float -> Offer a Zero
  AbsoluteDiscount :: Float -> Offer a Zero
  Restrict         :: Min (Succ n) p r => Vect (Succ n) a -> Offer a p -> Offer a r
  From             :: Integer -> Offer a p -> Offer a p
  Until            :: Integer -> Offer a p -> Offer a p
  Extend           :: Integer -> Offer a p -> Offer a p
  Both             :: Plus p q r => Offer a p -> Offer a q -> Offer a r
  BetterOf         :: Max p q r => Offer a p -> Offer a q -> Offer a r
  If               :: Max p q r => Expr a Bool -> Offer a p -> Offer a q -> Offer a r

o = Both p (BetterOf p (Both p p)) where p = Present 'a'

