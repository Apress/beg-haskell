module Chapter13.CheckPresents

data Expr : Type -> Type -> Type where
  AmountOf            : a -> Expr a Nat
  PriceOf             : a -> Expr a Float
  TotalNumberProducts : Expr a Nat
  TotalPrice          : Expr a Float
  Val                 : Num n => n -> Expr a n
  (:+:)               : Num n => Expr a n -> Expr a n -> Expr a n
  (:*:)               : Num n => Expr a n -> Expr a n -> Expr a n
  (:<:)               : Num n => Expr a n -> Expr a n -> Expr a Bool
  (:<=:)              : Num n => Expr a n -> Expr a n -> Expr a Bool
  (:>:)               : Num n => Expr a n -> Expr a n -> Expr a Bool
  (:>=:)              : Num n => Expr a n -> Expr a n -> Expr a Bool
  (:&&:)              : Expr a Bool -> Expr a Bool -> Expr a Bool
  (:||:)              : Expr a Bool -> Expr a Bool -> Expr a Bool
  Not                 : Expr a Bool -> Expr a Bool

data Offer : Type -> Nat -> Type where
  Present          : a -> Offer a 1
  PercentDiscount  : Float -> Offer a 0
  AbsoluteDiscount : Float -> Offer a 0
  Restrict         : Vect (S n) a -> Offer a p -> Offer a (minimum (S n) p)
  From             : Nat -> Offer a p -> Offer a p
  Until            : Nat -> Offer a p -> Offer a p
  Extend           : Nat -> Offer a p -> Offer a p
  Both             : Offer a p -> Offer a q -> Offer a (p + q)
  BetterOf         : Offer a p -> Offer a q -> Offer a (maximum p q)
  If               : Expr a Bool -> Offer a p -> Offer a q -> Offer a (maximum p q)

p : Offer Char 1
p = Present 'a'

