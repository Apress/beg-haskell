module Main where

import Control.Monad.Par
import System.Environment
import Control.DeepSeq

import Chapter8.APriori.Par
import Chapter8.APriori.Par2

main :: IO ()
main = do n <- fmap (read . head) getArgs
          print $ findTwoFactors n (n + 1)


{-
-- Version 1: no parallelism
findTwoFactors :: Integer -> Integer -> ([Integer],[Integer])
findTwoFactors x y = (findFactors x, findFactors y)
-}

-- Version 2: with futures
findTwoFactors :: Integer -> Integer -> ([Integer],[Integer])
findTwoFactors x y = runPar $ do
  factorsXVar <- spawnP $ findFactors x
  -- factorsYVar <- spawnP $ findFactors y
  -- factorsX <- get factorsXVar
  -- factorsY <- get factorsYVar
  let factorsY = findFactors y
      _        = rnf factorsY
  factorsX <- get factorsXVar
  return (factorsX, factorsY)

findFactors :: Integer -> [Integer]
findFactors 1 = [1]
findFactors n = let oneFactor = findFactor n 2
                 in oneFactor : (findFactors $ n `div` oneFactor)

findFactor :: Integer -> Integer -> Integer
findFactor n m | n == m         = n
               | n `mod` m == 0 = m
               | otherwise      = findFactor n (m + 1)
     
{-          
main :: IO ()
main = putStrLn $ printTicket 1 1 [(1,"A"),(2,"B")] [(1,"Machine"),(2,"Book")]
-}

printTicket :: Int -> Int -> [(Int,String)] -> [(Int,String)] -> String
printTicket idC idP clients products = runPar $ do
  clientV  <- new
  productV <- new
  fork $ lookupPar clientV  idC clients
  fork $ lookupPar productV idP products
  envV    <- new
  letterV <- new
  fork $ printEnvelope clientV envV
  fork $ printLetter   clientV productV letterV
  envS    <- get envV
  letterS <- get letterV
  return $ envS ++ "\n\n" ++ letterS
  
lookupPar :: (Eq a, NFData b) => IVar (Maybe b) -> a -> [(a,b)] -> Par ()
lookupPar i _ []                    = put i Nothing
lookupPar i x ((k,v):r) | x == k    = put i $ Just v
                        | otherwise = lookupPar i x r

printEnvelope :: IVar (Maybe String) -> IVar String -> Par ()
printEnvelope clientV envV = do
  clientName <- get clientV
  case clientName of
    Nothing -> put envV "Unknown"
    Just n  -> put envV $ "To: " ++ n

printLetter :: IVar (Maybe String) -> IVar (Maybe String) -> IVar String -> Par ()
printLetter clientV productV letterV = do
  clientName  <- get clientV
  productName <- get productV
  case (clientName, productName) of
    (Nothing, Nothing) -> put letterV "Unknown"
    (Just n,  Nothing) -> put letterV $ n ++ " bought something"
    (Nothing, Just p)  -> put letterV $ "Someone bought " ++ p
    (Just n,  Just p)  -> put letterV $ n ++ " bought " ++ p

