{-# LANGUAGE TemplateHaskell, QuasiQuotes,
             TypeFamilies, EmptyDataDecls,
             FlexibleContexts, GADTs,
             OverloadedStrings #-}

module Chapter13.Database where

import Database.Persist.TH

mkPersist sqlSettings [persistLowerCase|
Product
  name        String
  description String
  price       Double
  inStock     Int
  deriving Show
|]
