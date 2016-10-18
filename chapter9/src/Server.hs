{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.Trans
import qualified Data.ByteString.Char8 as BS
import Data.Conduit
import Data.Conduit.Network
import Data.Monoid
import System.Random
import Network

main :: IO ()
main = withSocketsDo $ runTCPServer (serverSettings 8900 HostAny) serverApp

serverApp :: Application IO
serverApp d = do appSource d $$ isWinner =$ appSink d

isWinner :: Conduit BS.ByteString IO BS.ByteString
isWinner = do client <- await
              case client of
                Nothing -> return ()
                Just c  -> do lift $ BS.putStrLn c
                              (w :: Bool) <- liftIO $ randomIO
                              (y :: Int ) <- liftIO $ randomRIO (0, 3000)
                              yield $ c <> BS.pack (" " ++ show w ++ " " ++ show y)
                              isWinner
