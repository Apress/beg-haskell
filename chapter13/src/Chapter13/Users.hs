{-# LANGUAGE EmptyDataDecls, GADTs #-}

module Chapter13.Users where

data AllowEverything
data AllowProducts
data AllowPurchases

data Person = Person { firstName :: String, lastName :: String }

data User r where
  Admin        :: Person -> User AllowEverything
  StoreManager :: Person -> User AllowEverything
  StorePerson  :: Person -> User AllowProducts
  Client       :: Person -> User AllowPurchases
