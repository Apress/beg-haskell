{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, TypeFamilies #-}

module Chapter11.Query where

import Control.Monad
import qualified Database.Persist as P  -- only Persistent
import Database.Esqueleto   -- Persistent + Esqueleto
import Database.Persist.Sqlite (runSqlite)

import Chapter11.Database
-- import Chapter11.Gender

getClientById :: (P.PersistQuery m, P.PersistMonadBackend m ~ P.PersistEntityBackend Client) => Int -> m (Maybe Client)
getClientById n = get $ Key (PersistInt64 $ fromIntegral n)

-- getPurchaseClient :: Purchase -> m (Maybe Client)
getPurchaseClient p = get (purchaseClient p)

-- getPurchaseClient' :: Int -> m (Maybe Client)
getPurchaseClient' pId = do p <- get $ Key (PersistInt64 $ fromIntegral pId)
                            case p of
                              Just p' -> get $ purchaseClient p'
                              Nothing -> return Nothing
                            
-- getClientByInfo :: String -> String -> String -> String -> m (Maybe Client)
getClientByInfo fName lName addr cnName = do
  cn <- getBy $ UniqueCountryName cnName
  case cn of
    Just (Entity cId _) -> 
      do cl <- getBy $ UniqueClient fName lName addr cId
         case cl of
           Just (Entity _ client) -> return $ Just client
           Nothing                -> return Nothing
    Nothing -> return Nothing

-- getAdultsOfSpainAndGermany :: m [Entity Client]
getAdultsOfSpainAndGermany = do
  Just (Entity spId _) <- getBy $ UniqueCountryName "Spain"
  Just (Entity geId _) <- getBy $ UniqueCountryName "Germany"
  P.selectList [ ClientCountry P.<-. [spId, geId], ClientAge P.>=. Just 18 ] []

-- countAdultsOfSpainAndGermany :: m Integer
countAdultsOfSpainAndGermany = do
  Just (Entity spId _) <- getBy $ UniqueCountryName "Spain"
  Just (Entity geId _) <- getBy $ UniqueCountryName "Germany"
  P.count [ ClientCountry P.<-. [spId, geId], ClientAge P.>=. Just 18 ]

-- getAdultsOfSpainAndUS :: m [Entity Client]
getAdultsOfSpainAndUS = do
  Just (Entity spId _) <- getBy $ UniqueCountryName "Spain"
  Just (Entity usId _) <- getBy $ UniqueCountryName "United States of America"
  P.selectList (     [ ClientCountry P.==. spId, ClientAge P.>=. Just 18 ]
               P.||. [ ClientCountry P.==. usId, ClientAge P.>=. Just 21 ] )
               [ P.Desc ClientAge ]

-- getProductsPage :: Int -> m [Entity Product]
getProductsPage n = P.selectList [ ] [ P.Asc ProductPrice, P.LimitTo 10, P.OffsetBy ((n-1)*10) ]
  
-- getCountriesWithBigBuyers :: m [Country]
getCountriesWithBigBuyers = do
  buyers <- P.selectKeysList [ ] [ ]
  buyersAndPurchases <- mapM (\b -> P.count [ PurchaseClient P.==. b ] >>= \c -> return (b,c)) buyers
  let buyersAndPurchases' = filter (\(_,c) -> c > 3) buyersAndPurchases
  mapM (\(b,_) -> do Just cl <- get b
                     Just cn <- get $ clientCountry cl
                     return cn)
       buyersAndPurchases'

-- getPeopleOver25 :: m [Entity Client]
getPeopleOver25 =
  select $
  from $ \client -> do
  where_ (client ^. ClientAge >. just (val 25))
  orderBy [ asc (client ^. ClientLastName), asc (client ^. ClientFirstName) ]
  return client

-- getPeopleOver25FromSpainOrGermany :: m [Entity Client]
getPeopleOver25FromSpainOrGermany =
  select $
  from $ \(client, country) -> do
  where_ (   client ^. ClientAge >. just (val 25)
         &&. country ^. CountryName `in_` valList [ "Spain", "Germany" ]
         &&. client ^. ClientCountry ==. country ^. CountryId )
  return client

-- getPeopleOver25FromSpainOrGermanyJoin :: m [Entity Client]
getPeopleOver25FromSpainOrGermanyJoin =
  select $ 
  from $ \(client `InnerJoin` country) -> do
  on (client ^. ClientCountry ==. country ^. CountryId)
  where_ (   client ^. ClientAge >. just (val 25)
         &&. country ^. CountryName `in_` valList [ "Spain", "Germany" ])
  orderBy [ asc (client ^. ClientLastName), asc (client ^. ClientFirstName) ]
  return client

-- getMoneyByClient :: m [(Entity Client, Value (Maybe Double))]
getMoneyByClient =
  select $
  from $ \(client `LeftOuterJoin` purchase) -> do
  on (client ^. ClientId ==. purchase ^. PurchaseClient)
  groupBy (client ^. ClientId)
  let s = sum_ (purchase ^. PurchaseAmount)
  return (client, s)
