module Chapter7.UnderAMonad where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer

addPrefix :: String -> Reader String String
addPrefix s = ask >>= \p -> return $  p ++ s
              -- ask >>= return . (++ s)

addPrefixL :: [String] -> Reader String [String]
addPrefixL = mapM addPrefix

logInformation :: [String] -> Writer String ()
logInformation = mapM_ (\s -> tell (s ++ "\n"))

logInformation2 :: [String] -> Writer String ()
logInformation2 infos = forM_ infos $ \s ->
                          tell (s ++ "\n")

factorialSteps :: Integer -> Writer (Sum Integer) Integer
factorialSteps n = foldM (\f x -> tell (Sum 1) >> return (f*x)) 1 [1 .. n]
                          