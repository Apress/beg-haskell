{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Chapter13.CategoriesFnDeps where

data TimeMachine = TimeMachine { model :: String } deriving Show
data Book = Book { title :: String, author :: String, rating :: Integer } deriving Show
data Costume = Costume { place :: String, range :: (Integer, Integer)} deriving Show

data TimeMachineOps = Travel Integer | Park deriving Show
data BookOps = Read | Highlight | WriteCritique deriving Show
data CostumeOps = PutOn | PutOff deriving Show

class Product p op | p -> op where
  price :: p -> Float
  perform :: p -> op -> String
  testOperation :: p -> op

instance Product TimeMachine TimeMachineOps where
  price _ = 1000.0
  perform (TimeMachine m) (Travel y) = "Travelling to " ++ show y ++ " with " ++ m
  perform (TimeMachine m) Park       = "Parking time machine " ++ m
  testOperation _ = Travel 0

{-
instance Product TimeMachine BookOps where
  price _         = 500.0
  perform _ _     = "What?!"
  testOperation _ = Read  -- ??
-}

performTest :: Product p op => p -> String
performTest p = perform p $ testOperation p

totalAmount :: Product p op => [p] -> Float
totalAmount = foldr (+) 0.0 . map price

