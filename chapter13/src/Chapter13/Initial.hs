module Chapter13.Initial where

data Offer a = Present a
  | PercentDiscount Float
  | AbsoluteDiscount Float
  | Restrict [a] (Offer a)
  | From Integer (Offer a)
  | Until Integer (Offer a)
  | Extend Integer (Offer a)
  | Both (Offer a) (Offer a)
  | BetterOf (Offer a) (Offer a)
  | If (Expr a) (Offer a) (Offer a)

data Expr a = AmountOf a
  | PriceOf a
  | TotalNumberProducts
  | TotalPrice
  | IVal Integer
  | FVal Float
  | (Expr a) :+: (Expr a)
  | (Expr a) :*: (Expr a)
  | (Expr a) :<: (Expr a)
  | (Expr a) :<=: (Expr a)
  | (Expr a) :>: (Expr a)
  | (Expr a) :>=: (Expr a)
  | (Expr a) :&&: (Expr a)
  | (Expr a) :||: (Expr a)
  | Not (Expr a)

noOffer :: Offer a
noOffer = AbsoluteDiscount 0

v :: Offer String
v = Until 30 $ BetterOf (AbsoluteDiscount 10.0)
                        (Both (Present "ballon")
                              (If (TotalPrice :>: IVal 100) (PercentDiscount 5.0)
                                  (AbsoluteDiscount 0)))

incorrectExpression :: Expr Char
incorrectExpression = TotalPrice :||: (TotalNumberProducts :<: PriceOf 'a')

data ExprR = EInt Integer | EFloat Float | EBool Bool

interpretExpr :: Expr a -> [(a,Float)] -> ExprR
interpretExpr (e1 :||: e2) list =
  case (interpretExpr e1 list, interpretExpr e2 list) of
    (EBool b1, EBool b2) -> EBool (b1 || b2)
    _                    -> error "type error"

