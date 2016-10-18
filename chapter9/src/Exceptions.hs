{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}

module Main where

import Control.Exception
import System.Random
-- import System.IO.Error
import System.IO
import System.Environment
import Data.Typeable

main :: IO ()

{-
main = do clients <- fmap lines $ readFile "clients.db"
          clientsAndWinners <- mapM (\c -> do (winner :: Bool) <- randomIO
                                              (year   :: Int ) <- randomRIO (0, 3000)
                                              return (c, winner, year))
                                    clients
          writeFile "clientsWinners.db" $ concatMap show clientsAndWinners
       `catch` (\(e :: IOException) -> if isDoesNotExistError e
                                          then putStrLn "File does not exist"
                                          else putStrLn $ "Other error: " ++ show e)
-}

{-
main = do (n1 :: Int) <- fmap read getLine
          (n2 :: Int) <- fmap read getLine
          putStrLn $ show n1 ++ " / " ++ show n2 ++ " = " ++ show (n1 `div` n2)
       `catch` (\(_ :: ErrorCall) -> putStrLn "Error reading number")
       `catch` (\(e :: ArithException) -> case e of
                  DivideByZero -> putStrLn "Division by zero"
                  _            -> putStrLn $ "Other arithmetic error: " ++ show e)
-}

{-
main = do (n1 :: Int) <- fmap read getLine
          (n2 :: Int) <- fmap read getLine
          putStrLn $ show n1 ++ " / " ++ show n2 ++ " = " ++ show (n1 `div` n2)
       `catches` [ Handler (\(_ :: ErrorCall) -> putStrLn "Error reading number")
                 , Handler (\(e :: ArithException) -> case e of
                                DivideByZero -> putStrLn "Division by zero"
                                _            -> putStrLn $ "Other arithmetic error: " ++ show e)]
-}

{-
main = handle (\(_ :: ErrorCall) -> putStrLn "Error reading number") $ 
       handle (\(e :: ArithException) -> putStrLn $ show e) $
       do (n1 :: Int) <- fmap read getLine
          (n2 :: Int) <- fmap read getLine
          putStrLn $ show n1 ++ " / " ++ show n2 ++ " = " ++ show (n1 `div` n2)
-}

{-
main = do r <- try (do (n1 :: Int) <- fmap read getLine
                       (n2 :: Int) <- fmap read getLine
                       return $ (n1, n2, n1 `div` n2) )
          case r of
            Right (n1, n2, q) -> putStrLn $ show n1 ++ " / " ++ show n2 ++ " = " ++ show q
            Left  (_ :: SomeException) -> putStrLn $ "Error in quotient"
-}

{-
main = catchJust (\e -> if e == DivideByZero then Just e else Nothing)
          (do (n1 :: Int) <- fmap read getLine
              (n2 :: Int) <- fmap read getLine
              putStrLn $ show n1 ++ " / " ++ show n2 ++ " = " ++ show (n1 `div` n2)
           `catch` (\(_ :: ErrorCall) -> putStrLn "Error reading number") )
          (\_ -> putStrLn "Division by zero")
-}

{-
main = do (inFile:outFile:_) <- getArgs
          inHandle  <- openFile inFile  ReadMode
          outHandle <- openFile outFile WriteMode
          ( loop inHandle outHandle
            `finally` (do hClose inHandle
                          hClose outHandle) )
-}

{-
main = do (inFile:outFile:_) <- getArgs
          bracket (openFile inFile  ReadMode)
                  hClose
                  (\inHandle -> bracket (openFile outFile WriteMode)
                                hClose
                                (\outHandle -> loop inHandle outHandle))
       where loop inHandle outHandle = undefined
-}

{-
main = do throw $ NoMethodError "I don't know what to do"
          `catch` (\(e :: SomeException) -> do putStr "An exception was thrown: "
                                               putStrLn $ show e)
-}

data AuthenticationException = UnknownUserName  String
                             | PasswordMismatch String
                             | NotEnoughRights  String
                             deriving (Show, Typeable)

instance Exception AuthenticationException

main = do throw $ UnknownUserName "Alejandro"
          `catch` (\(e :: AuthenticationException) -> do putStr "An exception was thrown: "
                                                         putStrLn $ show e)
