module Chapter14.Origami where

data Expr a = Plus  (Expr a) (Expr a)
            | Times (Expr a) (Expr a)
            | AmountOf a

foldExpr :: (b -> b -> b) -> (b -> b -> b) -> (a -> b) -> Expr a -> b
foldExpr plusFn timesFn amountFn e =
  let f = foldExpr plusFn timesFn amountFn
  in case e of
       Plus  e1 e2 -> plusFn  (f e1) (f e2)
       Times e1 e2 -> timesFn (f e1) (f e2)
       AmountOf x  -> amountFn x
       
performCount :: Eq a => [a] -> Expr a -> Int
performCount s = foldExpr (+) (*) (\x -> length $ filter (==x) s)

newtype ExprAlgebra a b = ExprAlgebra (b -> b -> b, b -> b -> b, a -> b)

foldExpr' :: ExprAlgebra a b -> Expr a -> b
foldExpr' a@(ExprAlgebra (plusFn,timesFn,amountFn)) e =
  case e of
    Plus  e1 e2 -> plusFn  (foldExpr' a e1) (foldExpr' a e2)
    Times e1 e2 -> timesFn (foldExpr' a e1) (foldExpr' a e2)
    AmountOf x  -> amountFn x

performCount' :: Eq a => [a] -> Expr a -> Int
performCount' s = foldExpr' $ ExprAlgebra ((+),(*),\x -> length $ filter (==x) s)

