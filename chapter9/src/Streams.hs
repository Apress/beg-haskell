{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module Main where

import System.IO
import Control.DeepSeq
import Data.Conduit
import qualified Data.Conduit.List as L
import Chapter9.Types
import System.Random
import Control.Monad.Trans
import Control.Monad.State
import qualified Data.ByteString.Char8 as BS
import Data.Monoid
import qualified Data.Conduit.Binary as B
-- import qualified Data.Binary as S
import qualified Data.Conduit.Serialization.Binary as S

main :: IO ()

{-
main = do h <- openFile "/home/serras/comandos" ReadMode
          s <- hGetContents h
          s `deepseq` hClose h
          print s
-}

{-
main = do p <- L.sourceList [ GovOrg 1 "Zas", Individual 2 (Person "Alejandro" "Serrano")] $$ people =$ L.consume
          print p
-}

{-
main = let clients = [ GovOrg 1 "Zas", Individual 2 (Person "Alejandro" "Serrano")]
           conduitGovOrgs = L.sourceList clients $$ countGovOrgs
        in print $ execState conduitGovOrgs 0
-}

{-
main = runResourceT $
  B.sourceFile "clients.db" $$ B.lines =$= winnersFile =$ B.sinkFile "clientsWinners.db"
-}

{-
main = runResourceT $ L.sourceList clients $$ S.conduitEncode =$ B.sinkFile "people.db"
       where clients = [ Person "Alejandro" "Serrano", Person "The Doctor" "Who?" ]
-}      


main = runResourceT $
  B.sourceFile "people.db" $$ S.conduitDecode 
                           =$ L.mapM_ (\(p :: Person) -> lift $ putStrLn $ show p)


people :: Monad m => Conduit (Client i) m Person
people = do client <- await
            case client of
              Nothing -> return ()
              Just c -> do case c of
                             Company { person = p }    -> yield p
                             Individual { person = p } -> yield p
                             _                         -> return ()
                           people

winners :: Conduit (Client i) IO (Client i, Bool, Int)
winners = do client <- await
             case client of
               Nothing -> return ()
               Just c  -> do (w :: Bool) <- lift $ randomIO
                             (y :: Int ) <- lift $ randomRIO (0, 3000)
                             yield (c, w, y)
                             winners

countGovOrgs :: MonadState Int m => Sink (Client i) m Int
countGovOrgs = do client <- await
                  case client of
                    Nothing -> do n <- lift $ get
                                  return n
                    Just c  -> do case c of
                                    GovOrg { } -> lift $ modify (+1)
                                    _          -> return ()
                                  countGovOrgs

winnersFile :: (Monad m, MonadIO m) => Conduit BS.ByteString m BS.ByteString
winnersFile = do client <- await
                 case client of
                   Nothing -> return ()
                   Just c  -> do (w :: Bool) <- liftIO $ randomIO
                                 (y :: Int ) <- liftIO $ randomRIO (0, 3000)
                                 yield $ c <> BS.pack (" " ++ show w ++ " " ++ show y)
                                 winnersFile

