{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Chapter3.Lists where

import Data.Function (on)
import Data.List
import Data.Ord
import Chapter3.ParamPoly

data InfNumber a = MinusInfinity
                 | Number a
                 | PlusInfinity
                 deriving Show

infMax MinusInfinity x       = x
infMax x MinusInfinity       = x
infMax PlusInfinity  _       = PlusInfinity
infMax _  PlusInfinity       = PlusInfinity
infMax (Number a) (Number b) = Number (max a b)

maximum' :: Ord t => [t] -> InfNumber t
maximum' = foldr (\x y -> infMax (Number x) y) MinusInfinity

maximum'' :: Ord t => [t] -> t
maximum'' = foldr1 max

bothFilters :: (a -> Bool) -> [a] -> ([a],[a])
bothFilters p list = (filter p list, filter (not . p) list)

skipUntilGov :: [Client a] -> [Client a]
skipUntilGov = dropWhile (\case { GovOrg {} -> False ; _ -> True })

isIndividual :: Client a -> Bool
isIndividual (Individual {}) = True
isIndividual _               = False

checkIndividualAnalytics :: [Client a] -> (Bool, Bool)
checkIndividualAnalytics cs = (any isIndividual cs, not $ all isIndividual cs)

compareClient :: Client a -> Client a -> Ordering
compareClient (Individual{person = p1}) (Individual{person = p2})
  = compare (firstName p1) (firstName p2)
compareClient (Individual {}) _ = GT
compareClient _ (Individual {}) = LT
compareClient c1 c2             = compare (clientName c1) (clientName c2)

listOfClients :: [Client Int]
-- listOfClients = [ Individual 2 (Person "James" "Joyce")
--                 , GovOrg 3 "NATO"
--                 , Company 4 "My Inc." (Person "Frank" "Kakfa") "Boss"
--                 , Individual 5 (Person "Albert" "Einstein")
--                 ]
listOfClients = [ Individual 2 (Person "H. G." "Wells")
                , GovOrg 3 "NTTF"  -- National Time Travel Foundation
                , Company 4 "Wormhole Inc." (Person "Karl" "Schwarzschild") "Physicist"
                , Individual 5 (Person "Doctor" "")
                , Individual 6 (Person "Sarah" "Jane")
                ]

enum :: Int -> Int -> [Int]
enum a b | a > b = []
enum a b         = a : enum (a+1) b

withPositions :: [a] -> [(Int,a)]
withPositions list = zip (enum 1 $ length list) list

companyDutiesAnalytics :: [Client a] -> [String]
companyDutiesAnalytics = map (duty . head) .
                           sortBy (flip  (compare `on` length)) .
                           groupBy ((==) `on` duty) .
                           filter isCompany
                         where isCompany (Company {}) = True
                               isCompany _            = False