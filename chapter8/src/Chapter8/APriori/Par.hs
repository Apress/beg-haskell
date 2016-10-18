module Chapter8.APriori.Par (apriori) where

import Control.Monad.Par
-- import Data.Set (Set)
import qualified Data.Set as S

import Chapter8.APriori.Types

{-
-- No parallelism
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
-}

noDups :: Ord a => [a] -> [a]
noDups = S.toList . S.fromList

-- With parMapM
apriori :: Double -> [Transaction] -> [FrequentSet]
apriori minSupport transactions = runPar $ do
  let c1 = noDups $ concatMap (\(Transaction t) -> map (FrequentSet . S.singleton) $ S.toList t) transactions
  l1NotFiltered <- parMap (\fs -> (fs, setSupport transactions fs > minSupport)) c1
  let l1 = concatMap (\(fs,b) -> if b then [fs] else []) l1NotFiltered
  return $ concat (l1 : generateMoreCs minSupport transactions l1)

generateMoreCs :: Double -> [Transaction] -> [FrequentSet] -> [[FrequentSet]]
generateMoreCs _ _ [] = []
generateMoreCs minSupport transactions lk =
  let ck1 = noDups $ zipWith (\(FrequentSet a) (FrequentSet b) -> FrequentSet $ a `S.union` b) lk lk
      lk1 = runPar $ filterLk minSupport transactions ck1
   in lk1 : generateMoreCs minSupport transactions lk1

-- Splitting in halves
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

{-
-- With skeleton
filterLk :: Double -> [Transaction] -> [FrequentSet] -> [FrequentSet]
filterLk minSupport transactions = 
  divConq (\p -> length p <= 5)
          (\p -> let (l, r) = splitAt (length p `div` 2) p in [l, r])
          (\[l,r] -> l ++ r)
          (filter (\fs -> setSupport transactions fs > minSupport)) 
-}

divConq :: NFData sol
  => (prob -> Bool)   -- indivisible?
  -> (prob -> [prob]) -- split into subproblems
  -> ([sol] -> sol)   -- join solutions
  -> (prob -> sol)    -- solve a subproblem
  -> (prob -> sol)
divConq indiv split join f prob = runPar $ go prob
  where go prob | indiv prob = return (f prob)
                | otherwise  = do sols <- parMapM go (split prob)
                                  return (join sols)

