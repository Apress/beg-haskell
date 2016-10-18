{-# LANGUAGE TypeFamilies #-}

module Chapter13.CategoriesTyFams where

data TimeMachine = TimeMachine { model :: String } deriving Show
data Book = Book { title :: String, author :: String, rating :: Integer } deriving Show
data Costume = Costume { place :: String, range :: (Integer, Integer)} deriving Show

data TimeMachineOps = Travel Integer | Park deriving Show
data BookOps = Read | Highlight | WriteCritique deriving Show
data CostumeOps = PutOn | PutOff deriving Show

class Product p where
  type Operation p :: *
  price :: p -> Float
  perform :: p -> Operation p -> String
  testOperation :: p -> Operation p

instance Product TimeMachine where
  type Operation TimeMachine = TimeMachineOps
  price _ = 1000.0
  perform (TimeMachine m) (Travel y) = "Travelling to " ++ show y ++ " with " ++ m
  perform (TimeMachine m) Park       = "Parking time machine " ++ m
  testOperation _ = Travel 0

instance Product Book where
  type Operation Book = BookOps
  price _ = 1.0
  perform (Book t _ _) _ = "Something on " ++ t
  testOperation _ = Read

performTest :: Product p => p -> String
performTest p = perform p $ testOperation p

totalAmount :: Product p => [p] -> Float
totalAmount = foldr (+) 0.0 . map price

-- performTestFromOther :: Product p => p -> p -> String
performTestFromOther :: (Product p, Product q, Operation p ~ Operation q) => p -> q -> String
performTestFromOther p q = perform p $ testOperation q

class Product2 p where
  data Operation2 p
  price2 :: p -> Float
  perform2 :: p -> Operation2 p -> String
  testOperation2 :: p -> Operation2 p

instance Product2 TimeMachine where
  data Operation2 TimeMachine = Travel2 Integer | Park2
  price2 _ = 1000.0
  perform2 (TimeMachine m) (Travel2 y) = "Travelling to " ++ show y ++ " with " ++ m
  perform2 (TimeMachine m) Park2       = "Parking time machine " ++ m
  testOperation2 _ = Travel2 0
