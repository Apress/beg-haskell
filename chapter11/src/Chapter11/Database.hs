{-# LANGUAGE TemplateHaskell, QuasiQuotes,
             TypeFamilies, EmptyDataDecls,
             FlexibleContexts, GADTs,
             OverloadedStrings #-}

module Chapter11.Database where

import Database.Persist.TH
import Data.Time.Clock

import Chapter11.Gender

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Country
  name      String
  canWeSend Bool default=False
  UniqueCountryName name
  deriving Show
Client
  firstName String
  lastName  String
  address   String
  country   CountryId
  gender    Gender Maybe
  age       Int Maybe
  UniqueClient firstName lastName address country
  deriving Show
Product
  name        String
  description String
  price       Double
  inStock     Int
  deriving Show
Purchase
  client  ClientId
  product ProductId
  number  Int
  amount  Double
  deriving Show
|]
