{-# LANGUAGE TypeFamilies, EmptyDataDecls, GADTs #-}

module Chapter13.CheckPresentsTyFams where

data Zero
data Succ n

type family Plus x y
type instance Plus Zero     x = x
type instance Plus (Succ x) y = Succ (Plus x y)

type family Min x y
type instance Min Zero y            = Zero
type instance Min (Succ x) Zero     = Zero
type instance Min (Succ x) (Succ y) = Succ (Min x y)

type family Max x y
type instance Max Zero y            = y
type instance Max (Succ x) Zero     = Succ x
type instance Max (Succ x) (Succ y) = Succ (Max x y)

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
  Restrict         :: Vect (Succ n) a -> Offer a p -> Offer a (Min (Succ n) p)
  From             :: Integer -> Offer a p -> Offer a p
  Until            :: Integer -> Offer a p -> Offer a p
  Extend           :: Integer -> Offer a p -> Offer a p
  Both             :: Offer a p -> Offer a q -> Offer a (Plus p q)
  BetterOf         :: Offer a p -> Offer a q -> Offer a (Max p q)
  If               :: Expr a Bool -> Offer a p -> Offer a q -> Offer a (Max p q)

o = Both p (BetterOf p (Both p p)) where p = Present 'a'

