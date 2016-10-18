{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
-- import Database.Persist  -- only Persistent
import Database.Esqueleto   -- Persistent + Esqueleto
import Database.Persist.Sqlite
import Data.Time.Clock

import Chapter11.Database
import Chapter11.Gender
import Chapter11.Query

main :: IO ()
main = do runSqlite "example.db" $ do
            -- Create table structure
            runMigration migrateAll
            -- Insert initial data 
            insertInitialData
            -- Run a query
            c1 <- getClientById 1
            liftIO $ print c1
            results1 <- getPeopleOver25FromSpainOrGermanyJoin
            mapM_ (\(Entity _ r) -> liftIO $ print r) results1
            results2 <- getMoneyByClient
            mapM_ (\(Entity _ r, Value l) -> liftIO $ print r >> print l) results2
            
mainWithExplicitConnection :: IO ()
mainWithExplicitConnection = withSqliteConn ":memory:" $ \conn ->
                                flip runSqlPersistM conn $ do
                                  runMigration migrateAll

mainWithExplicitPool :: IO ()
mainWithExplicitPool = withSqlitePool ":memory:" 3 $ \pool ->
                          flip runSqlPersistMPool pool $ do
                            runMigration migrateAll
  
insertInitialData :: PersistStore m => m ()
insertInitialData = do
  spain   <- insert $ Country "Spain" True
  germany <- insert $ Country "Germany" True
  uk      <- insert $ Country "United Kingdom" False
  _usa    <- insert $ Country "United States of America" False
  
  client1  <- insert $ Client "Alejandro" "Serrano" "Home Town, 1" spain (Just Male) Nothing
  client2  <- insert $ Client "Werner" "Heisenberg" "A place in Wurzburg" germany (Just Male) (Just 50)
  _client3 <- insert $ Client "Doctor" "Who" "Police Box" uk Nothing Nothing
  
  let longDescription = "blah blah blah"
  product1 <- insert $ Product "Travel to the XIX Century" longDescription 12.3 3
  product2 <- insert $ Product "TM-223 Machine" longDescription 1245.0 10
  
  _ <- insert $ Purchase client1 product1 1 20.0
  _ <- insert $ Purchase client1 product2 5 1002.3
  _ <- insert $ Purchase client2 product1 3 58.0
  
  return ()
