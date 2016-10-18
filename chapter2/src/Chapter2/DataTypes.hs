{-# LANGUAGE ViewPatterns, NamedFieldPuns, RecordWildCards #-}

module Chapter2.DataTypes where

import Data.Char

data Client = GovOrg     String
            | Company    String Integer Person String
            | Individual Person Bool
            deriving Show

data Person = Person String String Gender
            deriving Show

data Gender = Male | Female | Unknown
            deriving Show

clientName :: Client -> String
-- LONG VERSION
-- clientName client = case client of
--                       GovOrg  name                        -> name
--                       Company name _ _ _                  -> name
--                       Individual (Person fName lName _) _ -> fName ++ " " ++ lName
-- SHORT VERSION
clientName (GovOrg name)                         = name
clientName (Company name _ _ _)                  = name
clientName (Individual (Person fName lName _) _) = fName ++ " " ++ lName

responsibility :: Client -> String
responsibility (Company _ _ _ r) = r
responsibility _                 = "Unknown"

specialClient :: Client -> Bool
specialClient (clientName -> "Mr. Alejandro") = True
specialClient (responsibility -> "Director")  = True
specialClient _                               = False

companyName :: Client -> Maybe String
companyName client = case client of
                       Company name _ _ _ -> Just name
                       _                  -> Nothing
                       
fibonacci :: Integer -> Integer
-- LONG VERSION
-- fibonacci n = case n of
--                 0 -> 0
--                 1 -> 1
--                 _ -> fibonacci (n-1) + fibonacci (n-2)
-- SHORT VERSION
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

f :: Client -> String
f client = case client of
             Company _ _ (Person name _ _) "Boss" -> name ++ " is the boss"
             _                                  -> "There is no boss"

g :: Client -> String
g client = case client of
             Company _ _ (Person name _ _) pos -> 
               case pos of "Boss" -> name ++ " is the boss"
             _                               -> "There is no boss"

(+++) :: [a] -> [a] -> [a]
[]     +++ list2 = list2
(x:xs) +++ list2 = x:(xs +++ list2)
{-list1 +++ list2 = case list1 of
                    []   -> list2
                    x:xs -> x:(xs +++ list2)-}

sorted :: [Integer] -> Bool
sorted []  = True
sorted [_] = True
sorted (x : r@(y:_)) = x < y && sorted r

maxmin [x]    = (x,x)
maxmin (x:xs) = ( if x > xs_max then x else xs_max
                , if x < xs_min then x else xs_min
                ) where (xs_max, xs_min) = maxmin xs

ifibonacci :: Integer -> Maybe Integer
ifibonacci n | n < 0     = Nothing
ifibonacci 0             = Just 0
ifibonacci 1             = Just 1
ifibonacci n | otherwise = let (Just f1, Just f2) = (ifibonacci (n-1), ifibonacci (n-2))
                           in Just (f1 + f2)
{-ifibonacci n = case n of
                 n' | n' < 0    -> Nothing
                 0              -> Just 0
                 1              -> Just 1
                 _  | otherwise -> let Just f1 = ifibonacci (n-1)
                                       Just f2 = ifibonacci (n-2)
                                   in Just (f1 + f2)-}
{- ifibonacci n = if n < 0
               then Nothing
               else case n of
                     0 -> Just 0
                     1 -> Just 1
                     n -> let Just f1 = ifibonacci (n-1)
                              Just f2 = ifibonacci (n-2)
                          in Just (f1 + f2) -}

binom :: Integer -> Integer -> Integer
binom _ 0          = 1
binom x y | x == y = 1
binom n k          = (binom (n-1) (k-1)) + (binom (n-1) k)

multipleOf :: Integer -> Integer -> Bool
multipleOf x y = (mod x y) == 0

specialMultiples :: Integer -> String
specialMultiples n 
  | multipleOf n 2 = show n ++ " is multiple of 2"
  | multipleOf n 3 = show n ++ " is multiple of 3"
  | multipleOf n 5 = show n ++ " is multiple of 5"
  | otherwise      = show n ++ " is a beautiful number"
{-
specialMultiples n | multipleOf n 2 = show n ++ " is multiple of 2"
specialMultiples n | multipleOf n 3 = show n ++ " is multiple of 3"
specialMultiples n | multipleOf n 5 = show n ++ " is multiple of 5"
specialMultiples n | otherwise      = show n ++ " is a beautiful number"
-}
                       
data ClientR = GovOrgR  { clientRName :: String }
             | CompanyR { clientRName :: String
                        , companyId :: Integer
                        , person :: PersonR
                        , duty :: String }
             | IndividualR { person :: PersonR }
             deriving Show

data PersonR = PersonR { firstName :: String
                       , lastName :: String
                       } deriving Show

greet :: ClientR -> String
greet IndividualR { person = PersonR { .. } } = "Hi, " ++ firstName
greet CompanyR    { .. }                      = "Hello, " ++ clientRName
greet GovOrgR     { }                         = "Welcome"

nameInCapitals :: PersonR -> PersonR
nameInCapitals p@(PersonR { firstName = initial:rest }) = 
        let newName = (toUpper initial):rest
        in  p { firstName = newName }
nameInCapitals p@(PersonR { firstName = "" }) = p