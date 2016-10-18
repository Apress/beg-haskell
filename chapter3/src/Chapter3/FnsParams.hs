{-# LANGUAGE LambdaCase #-}

module Chapter3.FnsParams where

map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = (f x) : (map f xs)

apply3f2 :: (Integer -> Integer) -> Integer -> Integer
apply3f2 f x = 3 * f (x + 2)

equalTuples :: [(Integer,Integer)] -> [Bool]
equalTuples t = map (\(x,y) -> x == y) t

sayHello :: [String] -> [String]
sayHello names = map (\name -> case name of
                                 "Alejandro" -> "Hello, writer"
                                 _           -> "Welcome, " ++ name
                     ) names

sayHello' :: [String] -> [String]
sayHello' names = map (\case "Alejandro" -> "Hello, writer"
                             name        -> "Welcome, " ++ name
                      ) names

multiplyByN :: Integer -> (Integer -> Integer)
multiplyByN n = \x -> n*x

duplicateOdds :: [Integer] -> [Integer]
duplicateOdds list = map (*2) $ filter odd list

duplicateOdds' :: [Integer] -> [Integer]
duplicateOdds' = map (*2) . filter odd

--uncurry :: (a -> b -> c) -> (a,b) -> c
--uncurry f = \(x,y) -> f x y

--curry :: ((a,b) -> c) -> a -> b -> c
--curry f = \x y -> f (x,y)

(***) :: (a -> b) -> (c -> d) -> ((a,c) -> (b,d))
f *** g = \(x,y) -> (f x, g y)

duplicate :: a -> (a,a)
duplicate x = (x,x)

formula1 :: Integer -> Integer
formula1 = uncurry (+) . ( ((*7) . (+2)) *** (*3) ) . duplicate

maximum' :: [Integer] -> Integer
maximum' = foldr1 max
