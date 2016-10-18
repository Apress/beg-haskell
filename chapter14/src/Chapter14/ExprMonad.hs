module Chapter14.ExprMonad where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer

data Expr a = AmountOf a
  | PriceOf a
  | TotalNumberProducts
  | TotalPrice
  | IVal Int
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

exprExample :: Expr Char
exprExample = (AmountOf 'a' :<: IVal 2) :&&: (FVal 300.0 :<: TotalPrice)

productsExample :: [(Char,Float)]
productsExample = [('a',15.0), ('b',400.0)]

executeExpr :: Eq a => Expr a -> [(a, Float)] -> Result
executeExpr e p = execWriter (runReaderT (sem e) p)

newtype Result = Result (Maybe Int, Maybe Float, Maybe Bool) deriving Show
instance Monoid Result where
  _ `mappend` r2 = r2
  mempty         = Result (Nothing, Nothing, Nothing)

sem :: Eq a => Expr a -> ReaderT [(a,Float)] (Writer Result) ()
sem (AmountOf p) = do products <- ask
                      tell $ Result (Just (length $ filter (\(d,_) -> d == p) products), Nothing, Nothing)
sem (PriceOf p)  = do products <- ask
                      tell $ Result (Nothing, Just $ foldr (\(_,pr) x -> pr + x) 0.0 $ filter (\(d,_) -> d == p) products, Nothing)
sem TotalNumberProducts = do products <- ask
                             tell $ Result (Just (length products), Nothing, Nothing)
sem TotalPrice          = do products <- ask
                             tell $ Result (Nothing, Just (foldr (\(_,p) x -> p + x) 0.0 products), Nothing)
sem (IVal i) = tell $ Result (Just i, Nothing, Nothing)
sem (FVal f) = tell $ Result (Nothing, Just f, Nothing)
sem (e1 :+: e2) = do (_, Result (i1, f1, _)) <- listen (sem e1)
                     (_, Result (i2, f2, _)) <- listen (sem e2)
                     tell $ Result ((+) <$> i1 <*> i2, (+) <$> f1 <*> f2, Nothing)
sem (e1 :*: e2) = do (_, Result (i1, f1, _)) <- listen (sem e1)
                     (_, Result (i2, f2, _)) <- listen (sem e2)
                     tell $ Result ((*) <$> i1 <*> i2, (*) <$> f1 <*> f2, Nothing)
sem (e1 :<:  e2) = do (_, Result (i1, f1, _)) <- listen (sem e1)
                      (_, Result (i2, f2, _)) <- listen (sem e2)
                      tell $ Result (Nothing, Nothing, ((<) <$> i1 <*> i2) <|> ((<) <$> f1 <*> f2))
sem (e1 :<=: e2) = do (_, Result (i1, f1, _)) <- listen (sem e1)
                      (_, Result (i2, f2, _)) <- listen (sem e2)
                      tell $ Result (Nothing, Nothing, ((<=) <$> i1 <*> i2) <|> ((<=) <$> f1 <*> f2))
sem (e1 :>:  e2) = do (_, Result (i1, f1, _)) <- listen (sem e1)
                      (_, Result (i2, f2, _)) <- listen (sem e2)
                      tell $ Result (Nothing, Nothing, ((>) <$> i1 <*> i2) <|> ((>) <$> f1 <*> f2))
sem (e1 :>=: e2) = do (_, Result (i1, f1, _)) <- listen (sem e1)
                      (_, Result (i2, f2, _)) <- listen (sem e2)
                      tell $ Result (Nothing, Nothing, ((>=) <$> i1 <*> i2) <|> ((>=) <$> f1 <*> f2))
sem (e1 :&&: e2) = do (_, Result (_,_,b1)) <- listen (sem e1)
                      (_, Result (_,_,b2)) <- listen (sem e2)
                      tell $ Result (Nothing, Nothing, (&&) <$> b1 <*> b2)            
sem (e1 :||: e2) = do (_, Result (_,_,b1)) <- listen (sem e1)
                      (_, Result (_,_,b2)) <- listen (sem e2)
                      tell $ Result (Nothing, Nothing, (||) <$> b1 <*> b2) 
sem (Not e) = do (_, Result (_,_,b)) <- listen (sem e)
                 tell $ Result (Nothing, Nothing, not <$> b)                       


