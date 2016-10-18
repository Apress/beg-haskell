{-# LANGUAGE DataKinds, TypeFamilies, GADTs, UndecidableInstances, TemplateHaskell, QuasiQuotes #-}

module Chapter13.CheckRangesPromoted where

import Data.Singletons

$(singletons [d|
  data Nat = Zero | Succ Nat
           deriving (Show, Eq)
  |])

data BluNat (n :: Nat) where
  BluZero :: BluNat Zero
  BluSucc :: BluNat n -> BluNat (Succ n)

$(promote [d|
  plus :: Nat -> Nat -> Nat
  plus Zero     y = y
  plus (Succ x) y = Succ (plus x y)
  
  min :: Nat -> Nat -> Nat
  min Zero     _        = Zero
  min _        Zero     = Zero
  min (Succ x) (Succ y) = Succ (min x y)
  
  max :: Nat -> Nat -> Nat
  max Zero     y        = y
  max x        Zero     = x
  max (Succ x) (Succ y) = Succ (max x y)
  |])

$(promote [d|
  data Range = Empty | Open Nat | Closed Nat Nat
  
  infinite :: Range
  infinite = Open Zero
  |])
  
data Comparison = Less | Equal | Greater

$(promote [d|
  compare :: Nat -> Nat -> Comparison
  compare Zero     Zero     = Equal
  compare Zero     (Succ _) = Less
  compare (Succ _) Zero     = Greater
  compare (Succ x) (Succ y) = compare x y
 
  restrictFrom :: Nat -> Range -> Range
  restrictFrom _ Empty    = Empty
  restrictFrom n (Open f) = restrictFrom1 n f (compare n f)
  restrictFrom n (Closed f t) = restrictFrom2 n f t (compare n f) (compare n t)
  
  restrictFrom1 :: Nat -> Nat -> Comparison -> Range
  restrictFrom1 n _ Greater = Open n
  restrictFrom1 _ f Equal   = Open f
  restrictFrom1 _ f Less    = Open f
  
  restrictFrom2 :: Nat -> Nat -> Nat -> Comparison -> Comparison -> Range
  restrictFrom2 _ _ _ Greater Greater = Empty
  restrictFrom2 _ _ _ Greater Equal   = Empty
  restrictFrom2 n _ t Greater Less    = Closed n t
  restrictFrom2 _ f t Equal   _       = Closed f t
  restrictFrom2 _ f t Less    _       = Closed f t
  
  -- Exercise
  restrictUntil :: Nat -> Range -> Range
  restrictUntil _ Empty    = Empty
  restrictUntil n (Open f) = restrictUntil1 n f (compare n f)
  restrictUntil n (Closed f t) = restrictUntil2 n f t (compare n f) (compare n t)
  
  restrictUntil1 :: Nat -> Nat -> Comparison -> Range
  restrictUntil1 n f Greater = Closed f n
  restrictUntil1 _ _ Equal   = Empty
  restrictUntil1 _ _ Less    = Empty
  
  restrictUntil2 :: Nat -> Nat -> Nat -> Comparison -> Comparison -> Range
  restrictUntil2 _ f t _       Greater = Closed f t
  restrictUntil2 _ f t _       Equal   = Closed f t
  restrictUntil2 n f _ Greater Less    = Closed f n
  restrictUntil2 _ _ _ Equal   Less    = Empty
  restrictUntil2 _ _ _ Less    Less    = Empty
  
  intersect :: Range -> Range -> Range
  intersect Empty          _  = Empty
  intersect (Open f1)      d2 = restrictFrom f1 d2
  intersect (Closed f1 t1) d2 = restrictFrom f1 (restrictUntil t1 d2)
  |])

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

data Vect (n :: Nat) a where
  VNil  :: Vect Zero a
  VCons :: a -> Vect n a -> Vect (Succ n) a

data Offer a (r :: Range) where
  Present          :: a -> Offer a Infinite
  PercentDiscount  :: Float -> Offer a Infinite
  AbsoluteDiscount :: Float -> Offer a Infinite
  Restrict         :: Vect (Succ n) a -> Offer a d -> Offer a d
  -- From             :: (n :: Nat) -> Offer a d -> Offer a (RestrictFrom n d)
  -- Until            :: (n :: Nat) -> Offer a d -> Offer a (RestrictUntil n d)
  From             :: SNat n -> Offer a d -> Offer a (RestrictFrom n d)
  Until            :: SNat n -> Offer a d -> Offer a (RestrictUntil n d)
  Both             :: Offer a d1 -> Offer a d2 -> Offer a (Intersect d1 d2)
  BetterOf         :: Offer a d1 -> Offer a d2 -> Offer a (Intersect d1 d2)
  If               :: Expr a Bool -> Offer a d1 -> Offer a d2 -> Offer a (Intersect d1 d2)

zero :: SNat Zero
zero = sing
one :: SNat (Succ Zero)
one = sing
two :: SNat (Succ (Succ Zero))
two = sing
three :: SNat (Succ (Succ (Succ Zero)))
three = sing
four :: SNat (Succ (Succ (Succ (Succ Zero))))
four = sing
five :: SNat (Succ (Succ (Succ (Succ (Succ Zero)))))
five = sing

o = Both (From one p) (BetterOf p (Until three p)) where p = Present 'a'

