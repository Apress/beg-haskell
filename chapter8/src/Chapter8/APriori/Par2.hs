module Chapter8.APriori.Par2 where

import Control.Monad (guard)
import Control.Monad.Par
-- import Data.Set (Set)
import qualified Data.Set as S
import Data.List (unfoldr)

import Chapter8.APriori.Types

apriori2 :: Double -> Double -> [Transaction] -> [AssocRule]
-- apriori2 minSupport minConfidence transactions =
--   let l1 = generateL1 minSupport transactions
--       allL = concat $ unfoldr (generateNextLk minSupport transactions) (1, l1)
--    in generateAssocRules minConfidence transactions allL
apriori2 minSupport minConfidence transactions = 
  generateAssocRules minConfidence transactions
    $ concat $ unfoldr (generateNextLk minSupport transactions)
                       (1, generateL1 minSupport transactions)

generateL1 :: Double -> [Transaction] -> [FrequentSet]
generateL1 minSupport transactions = runPar $ do
  let c1 = noDups $ concatMap (\(Transaction t) -> map (FrequentSet . S.singleton) $ S.toList t) transactions
  l1NotFiltered <- parMap (\fs -> (fs, setSupport transactions fs > minSupport)) c1
  return $ concatMap (\(fs,b) -> if b then [fs] else []) l1NotFiltered
{-
  let c1 = noDups $ concatMap (\(Transaction t) -> map (FrequentSet . S.singleton) $ S.toList t) transactions
      l1NotFiltered = map (\fs -> (fs, setSupport transactions fs > minSupport)) c1
   in concatMap (\(fs,b) -> if b then [fs] else []) l1NotFiltered
-}
{-
  noDups $ do Transaction t <- transactions
              e <- S.toList t
              let fs = FrequentSet $ S.singleton e
              guard $ setSupport transactions fs > minSupport
              return fs
-}

generateNextLk :: Double -> [Transaction] -> (Int, [FrequentSet]) -> Maybe ([FrequentSet], (Int, [FrequentSet]))
generateNextLk _ _ (_, []) = Nothing
generateNextLk minSupport transactions (k, lk) =
  let ck1 = noDups $ [ FrequentSet $ a `S.union` b | FrequentSet a <- lk, FrequentSet b <- lk
                                                   , S.size (a `S.intersection` b) == k - 1 ]
      lk1 = runPar $ filterLk minSupport transactions ck1
   in Just (lk1, (k+1, lk1))

filterLk :: Double -> [Transaction] -> [FrequentSet] -> Par [FrequentSet]
filterLk minSupport transactions ck =
  let lengthCk = length ck
   in if lengthCk <= 5
      then return $ filter (\fs -> setSupport transactions fs > minSupport) ck
      else let (l,r) = splitAt (lengthCk `div` 2) ck
            in do lVar <- spawn $ filterLk minSupport transactions l
                  lFiltered <- get lVar
                  rVar <- spawn $ filterLk minSupport transactions r
                  rFiltered <- get rVar
                  return $ lFiltered ++ rFiltered

generateAssocRules :: Double -> [Transaction] -> [FrequentSet] -> [AssocRule]
generateAssocRules minConfidence transactions sets =
  do FrequentSet fs <- sets
     subset@(_:_) <- powerset $ S.toList fs
     let ssubset = S.fromList subset
         rule = AssocRule ssubset (fs `S.difference` ssubset)
     guard $ ruleConfidence transactions rule > minConfidence
     return rule

noDups :: Ord a => [a] -> [a]
noDups = S.toList . S.fromList

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)