module Chapter7.APriori.FirstImpl (apriori) where

-- import Data.Set (Set)
import qualified Data.Set as S

import Chapter7.APriori.Types

apriori :: Double -> [Transaction] -> [FrequentSet]
apriori minSupport transactions =
  let c1 = noDups $ concatMap (\(Transaction t) -> map (FrequentSet . S.singleton) $ S.toList t) transactions
      l1 = filter (\fs -> setSupport transactions fs > minSupport) c1
   in concat $ l1 : generateMoreCs minSupport transactions l1

generateMoreCs :: Double -> [Transaction] -> [FrequentSet] -> [[FrequentSet]]
generateMoreCs _ _ [] = []
generateMoreCs minSupport transactions lk =
  let ck1 = noDups $ zipWith (\(FrequentSet a) (FrequentSet b) -> FrequentSet $ a `S.union` b) lk lk
      lk1 = filter (\fs -> setSupport transactions fs > minSupport) ck1
   in lk1 : generateMoreCs minSupport transactions lk1

noDups :: Ord a => [a] -> [a]
noDups = S.toList . S.fromList
