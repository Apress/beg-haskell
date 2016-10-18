{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, TemplateHaskell, DoAndIfThenElse #-}

module Main where

import Control.Concurrent
import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad
import System.Environment
import System.Random

-- For deriving Serializable
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

data GalaxyMessage = LookForGalaxy ProcessId
-- data GalaxyMessage = LookForGalaxy (SendPort GalaxyMessage)
                   | GalaxyFound String
                   deriving (Typeable, Generic)
instance Binary GalaxyMessage

data WormHoleMessage = LostInWormHole
                     deriving (Typeable, Generic)
instance Binary WormHoleMessage

-- Version 1: one message
{-
traveller :: Process ()
traveller = do LookForGalaxy m <- expect
               say "Looking for galaxy"
               r <- liftIO $ randomRIO (3, 15)
               liftIO $ threadDelay (r * 1000000)
               say "Found!"
               send m (GalaxyFound "Andromeda")

remotable ['traveller]

master :: [NodeId] -> Process ()
master nodes = 
  do myPid <- getSelfPid
     mapM_ (\node -> do say $ "Sending to " ++ show node
                        pid <- spawn node $(mkStaticClosure 'traveller)
                        send pid (LookForGalaxy myPid))
           nodes
     forever $ do GalaxyFound g <- expect
                  say $ "Found galaxy: " ++ g
-}

-- Version 2: several kinds of message

traveller :: Process ()
traveller = do LookForGalaxy m <- expect
               say "Looking for galaxy"
               b <- liftIO $ randomIO
               if b
               then do say "Found!"
                       send m (GalaxyFound "Andromeda")
               else do say "Lost in wormhole!"
                       send m LostInWormHole

remotable ['traveller]

master :: [NodeId] -> Process ()
master nodes = 
  do myPid <- getSelfPid
     mapM_ (\node -> do say $ "Sending to " ++ show node
                        pid <- spawn node $(mkStaticClosure 'traveller)
                        send pid (LookForGalaxy myPid))
           nodes
     forever $ do receiveWait 
                    [ match $ \(GalaxyFound g) -> say $ "Found galaxy: " ++ g
                    , match $ \LostInWormHole  -> say "Lost in wormhole"
                    ]


-- Version 3: using channels
{-
traveller :: Process ()
traveller = do LookForGalaxy sendPort <- expect
               say "Looking for galaxy"
               say "Found!"
               sendChan sendPort (GalaxyFound "Andromeda")

remotable ['traveller]

master :: [NodeId] -> Process ()
master = 
  mapM_ $ \node -> do say $ "Creating channel with " ++ show node
                      pid <- spawn node $(mkStaticClosure 'traveller)
                      (sendPort, rcvPort) <- newChan
                      send pid (LookForGalaxy sendPort)      -- init conversation
                      GalaxyFound g <- receiveChan rcvPort   -- it could be more fruitful
                      say $ "Found galaxy: " ++ g
-}
main :: IO ()
main = do args <- getArgs
          case args of
            ["master", host, port] -> do
              backend <- initializeBackend host port (Main.__remoteTable initRemoteTable)
              putStrLn "Starting master..."
              startMaster backend master
            ["traveller", host, port] -> do
              backend <- initializeBackend host port (Main.__remoteTable initRemoteTable)
              putStrLn "Starting traveller..."
              startSlave backend
            _ -> do putStrLn "Unknown parameters"



-- Speak about http://www.well-typed.com/blog/71
