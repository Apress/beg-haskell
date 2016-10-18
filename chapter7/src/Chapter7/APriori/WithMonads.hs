module Chapter7.APriori.WithMonads where

import Control.Monad (guard)
-- import Data.Set (Set)
import qualified Data.Set as S
import Data.List (unfoldr)

import Chapter7.APriori.Types

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
generateL1 minSupport transactions =
  noDups $ do Transaction t <- transactions
              e <- S.toList t
              let fs = FrequentSet $ S.singleton e
              guard $ setSupport transactions fs > minSupport
              return fs

generateNextLk :: Double -> [Transaction] -> (Int, [FrequentSet]) -> Maybe ([FrequentSet], (Int, [FrequentSet]))
generateNextLk _ _ (_, []) = Nothing
generateNextLk minSupport transactions (k, lk) =
  let lk1 = noDups $ do FrequentSet a <- lk
                        FrequentSet b <- lk
                        guard $ S.size (a `S.intersection` b) == k - 1
                        let fs = FrequentSet $ a `S.union` b
                        guard $ setSupport transactions fs > minSupport
                        return fs
   in Just (lk1, (k+1, lk1))


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
