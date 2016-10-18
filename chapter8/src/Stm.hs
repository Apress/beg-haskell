module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Monad
import System.Random

-- Basic combinators
forkN :: Int -> IO () -> IO ()
-- forkN 0 _ = return ()
-- forkN n f = forkIO f >> forkN (n - 1) f
forkN n f = replicateM_ n (forkIO f)

forkDelay :: Int -> IO () -> IO ()
forkDelay n f = replicateM_ n $ forkIO (randomDelay >> f)

randomDelay :: IO ()
randomDelay = do r <- randomRIO (3, 15)
                 threadDelay (r * 1000000)

-- Example 1
{-
main :: IO ()
main = do v <- newMVar 10000
          forkDelay 5 $ updateMoney v
          forkDelay 5 $ readMoney v
          _ <- getLine  -- to wait for completion
          return ()
-}

updateMoney :: MVar Integer -> IO ()
updateMoney v = do r <- randomRIO (0, 3000)
                   m <- takeMVar v
                   putStrLn $ "Updating value, which is " ++ show m
                   putMVar v (m + r)

readMoney :: MVar Integer -> IO ()
readMoney v = do m <- readMVar v  -- equivalent to take and put
                 putStrLn $ "The current value is " ++ show m


-- Example 2
{-
main :: IO ()
main = do v <- newMVar 10000
          s <- newMVar [("a",7)]
          forkDelay 5 $ updateMoneyAndStock "a" 1000 v s
          forkDelay 5 $ printMoneyAndStock v s
          _ <- getLine  -- to wait for completion
          return ()
-}

updateMoneyAndStock :: Eq a => a -> Integer -> MVar Integer -> MVar [(a,Integer)] -> IO ()
updateMoneyAndStock product price money stock =
  do s <- takeMVar stock
     let Just productNo = lookup product s
     if productNo > 0
       then do m <- takeMVar money    -- Problem of deadlocking
               let newS = map (\(k,v) -> if k == product then (k,v-1) else (k,v)) s
               putMVar money (m + price) >> putMVar stock newS  -- Problem of deadlocking
       else putMVar stock s

printMoneyAndStock :: Show a => MVar Integer -> MVar [(a,Integer)] -> IO ()
printMoneyAndStock money stock = do m <- readMVar money  -- Problem of deadlocking
                                    s <- readMVar stock
                                    putStrLn $ show m ++ "\n" ++ show s

-- Example 3
{-
main :: IO ()
main = do v <- newTVarIO 10000
          s <- newTVarIO [("a",7)]
          forkDelay 5 $ atomically $ updateMoneyAndStockStm "a" 1000 v s
          _ <- getLine  -- to wait for completion
          return ()
-}

updateMoneyAndStockStm :: Eq a => a -> Integer -> TVar Integer -> TVar [(a,Integer)] -> STM ()
updateMoneyAndStockStm product price money stock =
  do s <- readTVar stock
     let Just productNo = lookup product s
     if productNo > 0
       then do m <- readTVar money
               let newS = map (\(k,v) -> if k == product then (k,v-1) else (k,v)) s
               writeTVar money (m + price) >> writeTVar stock newS
       else return ()

payByCard :: Eq a => a -> Integer -> TVar Integer -> TVar [(a,Integer)] -> STM ()
payByCard product price money stock =
  do working <- isCardSystemWorking
     if not working
       then retry  -- shows retry
       else updateMoneyAndStockStm product price money stock  -- shows compositionality

isCardSystemWorking :: STM Bool
isCardSystemWorking = undefined

-- Merging if retry
pay :: Eq a => a -> Integer -> TVar Integer -> TVar [(a,Integer)] -> STM ()
pay product price money stock = payByCard product price money stock `orElse`
                                payByCash product price money stock

payByCash :: Eq a => a -> Integer -> TVar Integer -> TVar [(a,Integer)] -> STM ()
payByCash = undefined

--Use a queue

main :: IO ()
main = do q <- newTQueueIO
          forkIO   $ backend q
          forkN 10 $ frontend q
          _ <- getLine
          return ()


backend :: TQueue (String,Integer) -> IO ()
backend q = do 
  m <- newTVarIO 10000
  s <- newTVarIO [("a",7)]
  forever $ atomically $ do (product,price) <- readTQueue q
                            pay product price m s

frontend :: TQueue (String,Integer) -> IO ()
frontend q = do (product,price) <- askClient
                atomically $ writeTQueue q (product,price)

askClient :: IO (String,Integer)
askClient = undefined
