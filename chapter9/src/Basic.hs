{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Chapter9.Types
import System.Console.Haskeline
import Data.Maybe
import Control.Monad
import Control.Monad.Loops
import System.Random
import Data.List
import Data.String
import System.Environment
import System.IO

-- Different examples
main :: IO ()

main = putStrLn "Hello Beginning Haskell!"

{-
main = do putStrLn "Where do you want to travel?"
          place <- getLine
          let year = (length place) * 10
          putStrLn $ "You should travel to year " ++ show year
-}

{-
main = do putStrLn "First name?"
          fName <- getLine
          putStrLn "Last name?"
          lName <- getLine
          putChar '>' >> putChar ' '
          print $ Person fName lName
-}

{-
main = runInputT defaultSettings $ do
  fName <- getInputLine "First name? "
  lName <- getInputLine "Last name? "
  case (fName, lName) of
    (Just f, Just l) -> outputStrLn $ show (Person f l)
    (_     , _     ) -> outputStrLn "I cannot identify you"
-}

createVIPList :: Show a => [Client a] -> IO [Client a]
createVIPList = foldM (\lst c -> do
                         putStr "\nShould "
                         putStr $ show c
                         putStrLn " be included in the VIP list? "
                         answer <- getLine
                         case answer of
                           'Y':_ -> return $ c:lst
                           _     -> return lst) []

{-
main = do actionName <- getLine
          case lookup actionName listOfActions of
            Just action -> action
            Nothing     -> putStrLn "Unknown action"
-}

listOfActions :: [(String, IO ())]
listOfActions = [
  ("greet", do name <- getLine
               putStrLn $ "Hello " ++ name),
  ("sum"  , do putStrLn "First number:"
               n1 <- fmap read getLine
               putStrLn "Second number:"
               n2 <- fmap read getLine
               putStrLn $ show n1 ++ "+" ++ show n2 ++ "=" ++ show (n1+n2))]

{-
main = do (initial :: Int) <- fmap read getLine
          jumps <- unfoldrM (\_ -> do next <- randomRIO (0, 3000)
                                      if next == initial 
                                         then return Nothing
                                         else return $ Just (next, next))
                            initial
          print $ take 10 jumps
-}

{-
main = do (initial :: Int) <- fmap read getLine
          gen <- getStdGen
          print $ take 10 $ getJumps gen initial
-}

getJumps :: StdGen -> Int -> [Int]
getJumps gen initial = unfoldr (\g -> let (next, nextG) = randomR (0, 3000) g in
                                      if next == initial
                                         then Nothing
                                         else Just (next, nextG))
                               gen

{-
main = do clients <- fmap lines $ readFile "clients.db"
          clientsAndWinners <- mapM (\c -> do (winner :: Bool) <- randomIO
                                              (year   :: Int ) <- randomRIO (0, 3000)
                                              return (c, winner, year))
                                    clients
          writeFile "clientsWinners.db" $ concatMap show clientsAndWinners
-}

{-
main = do (inFile:outFile:_) <- getArgs
          inHandle  <- openFile inFile  ReadMode
          outHandle <- openFile outFile WriteMode
          loop inHandle outHandle
          hClose inHandle
          hClose outHandle
       where loop inHandle outHandle = do
               isEof <- hIsEOF inHandle
               if not isEof
                  then do client <- hGetLine inHandle
                          (winner :: Bool) <- randomIO
                          (year   :: Int ) <- randomRIO (0, 3000)
                          hPutStrLn outHandle $ show (client, winner, year)
                          loop inHandle outHandle
-}

{-
main = do (inFile:outFile:_) <- getArgs
          withFile inFile  ReadMode  $ \inHandle ->
            withFile outFile WriteMode $ \outHandle ->
              loop inHandle outHandle
       where loop inHandle outHandle = do
               isEof <- hIsEOF inHandle
               if not isEof
                  then do client <- hGetLine inHandle
                          (winner :: Bool) <- randomIO
                          (year   :: Int ) <- randomRIO (0, 3000)
                          hPutStrLn outHandle $ show (client, winner, year)
                          loop inHandle outHandle
                  else return ()
-}

          
