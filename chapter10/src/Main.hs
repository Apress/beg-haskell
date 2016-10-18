{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Control.Monad.Trans
import Data.Conduit
import qualified Data.Conduit.Binary as B
import qualified Data.Conduit.List as L
import qualified Data.Conduit.Text as T
import Data.Monoid
import Data.Text
import System.Random

import Chapter10.Builder
import Chapter10.Parser
import Chapter10.TypeClasses

main :: IO()
main = runResourceT $
  B.sourceFile "clients.db" $$ T.decode T.utf8 =$=
  T.lines =$= winnersFile =$= L.concatMap (\x -> [x, "\n"]) =$=
  T.encode T.utf8 =$ B.sinkFile "clientsWinners.db"

winnersFile :: (Monad m, MonadIO m) => Conduit Text m Text
winnersFile = do client <- await
                 case client of
                   Nothing -> return ()
                   Just c  -> do (w :: Bool) <- liftIO $ randomIO
                                 (y :: Int ) <- liftIO $ randomRIO (0, 3000)
                                 yield $ c <> " " <> (pack $ show w) <> " " <> (pack $ show y)
                                 winnersFile
