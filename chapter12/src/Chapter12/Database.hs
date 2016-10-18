{-# LANGUAGE TemplateHaskell, QuasiQuotes,
             TypeFamilies, EmptyDataDecls,
             FlexibleContexts, GADTs,
             OverloadedStrings #-}

module Chapter12.Database where

import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Country json
  name      String
  canWeSend Bool default=False
  UniqueCountryName name
  deriving Show
Client json
  firstName String
  lastName  String
  address   String
  country   CountryId
  age       Int Maybe
  UniqueClient firstName lastName address country
  deriving Show
Product json
  name        String
  description String
  price       Double
  inStock     Int
  deriving Show
Purchase json
  client  ClientId
  product ProductId
  number  Int
  amount  Double
  deriving Show
|]
