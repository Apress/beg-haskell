{-# LANGUAGE GADTs #-}

module Chapter13.GADTs where

data Offer a = Present a
             | PercentDiscount Float
             | AbsoluteDiscount Float
             | Restrict [a] (Offer a)
             | From Integer (Offer a)
             | Until Integer (Offer a)
             | Extend Integer (Offer a)
             | Both (Offer a) (Offer a)
             | BetterOf (Offer a) (Offer a)
             | If (Expr a Bool) (Offer a) (Offer a)

data Expr a r where
  AmountOf            :: a -> Expr a Integer
  PriceOf             :: a -> Expr a Float
  TotalNumberProducts :: Expr a Integer
  TotalPrice          :: Expr a Float
  {- -- Not overloaded versions of values
  IVal                :: Integer -> Expr a Integer
  FVal                :: Float -> Expr a Float
  -}
  Val                 :: Num n => n -> Expr a n
  {- -- Not overloaded versions of comparisons
  (:+:)               :: Expr a Integer -> Expr a Integer -> Expr a Integer
  (:+.:)              :: Expr a Float -> Expr a Float -> Expr a Float
  (:*:)               :: Expr a Integer -> Expr a Integer -> Expr a Integer
  (:*.:)              :: Expr a Float -> Expr a Float -> Expr a Float
  (:<:)               :: Expr a Integer -> Expr a Integer -> Expr a Bool
  (:<.:)              :: Expr a Float -> Expr a Float -> Expr a Bool
  (:<=:)              :: Expr a Integer -> Expr a Integer -> Expr a Bool
  (:<=.:)             :: Expr a Float -> Expr a Float -> Expr a Bool
  (:>:)               :: Expr a Integer -> Expr a Integer -> Expr a Bool
  (:>.:)              :: Expr a Float -> Expr a Float -> Expr a Bool
  (:>=:)              :: Expr a Integer -> Expr a Integer -> Expr a Bool
  (:>=.:)             :: Expr a Float -> Expr a Float -> Expr a Bool
  -}
  -- Overloaded versions of comparisons
  (:+:)               :: Num n => Expr a n -> Expr a n -> Expr a n
  (:*:)               :: Num n => Expr a n -> Expr a n -> Expr a n
  (:<:)               :: Num n => Expr a n -> Expr a n -> Expr a Bool
  (:<=:)              :: Num n => Expr a n -> Expr a n -> Expr a Bool
  (:>:)               :: Num n => Expr a n -> Expr a n -> Expr a Bool
  (:>=:)              :: Num n => Expr a n -> Expr a n -> Expr a Bool
  (:&&:)              :: Expr a Bool -> Expr a Bool -> Expr a Bool
  (:||:)              :: Expr a Bool -> Expr a Bool -> Expr a Bool
  Not                 :: Expr a Bool -> Expr a Bool

{- Does not compile
incorrectExpression :: Expr Char Bool
incorrectExpression = TotalPrice :||: (TotalNumberProducts :<: PriceOf 'a')
-}

interpretExpr :: Expr a t -> [(a,Float)] -> t
interpretExpr (e1 :+: e2)  list = interpretExpr e1 list + interpretExpr e2 list
interpretExpr (e1 :||: e2) list = interpretExpr e1 list || interpretExpr e2 list

