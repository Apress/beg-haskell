{-# LANGUAGE FlexibleInstances, FlexibleContexts, DeriveGeneric, OverloadedStrings #-}

module Chapter10.Types where

import Data.Aeson
import Data.Aeson.Types
import Control.Lens ((^?))
import Control.Lens.Aeson
import Data.Text
import GHC.Generics
import Control.Applicative

import Data.Conduit
import qualified Data.Conduit.Binary as B
import qualified Data.Conduit.List as L
import qualified Data.ByteString.Lazy as LB

import qualified Data.HashMap.Strict as M

data Client i = GovOrg  { clientId :: i, clientName :: String }
              | Company { clientId :: i, clientName :: String
                         , person :: Person, duty :: String }
              | Individual { clientId :: i, person :: Person }
              deriving (Show, Generic)

data Person = Person { firstName :: String, lastName  :: String }
              deriving (Show, Read, Generic)

{-
instance ToJSON i   => ToJSON   (Client i)
instance FromJSON i => FromJSON (Client i)
instance ToJSON   Person
instance FromJSON Person
-}

clientToJSON :: Client Integer -> Value
clientToJSON (GovOrg i n) =
  object [ "type"   .= String "govorg"
         , "id"     .= Number (fromInteger i)
         , "name"   .= String (pack n) ]
clientToJSON (Company i n p d) =
  object [ "type"   .= String "company"
         , "id"     .= Number (fromInteger i)
         , "name"   .= String (pack n)
         , "person" .= personToJSON p
         , "duty"   .= String (pack d) ]
clientToJSON (Individual i p) =
  object [ "type"   .= String "individual"
         , "id"     .= Number (fromInteger i)
         , "person" .= personToJSON p ]

personToJSON :: Person -> Value
personToJSON (Person f l) = object [ "first" .= String (pack f)
                                   , "last"  .= String (pack l) ]
{-
jsonToPerson :: Value -> Maybe Person
jsonToPerson (Object o) = do String f <- M.lookup "first" o
                             String l <- M.lookup "last"  o
                             return $ Person (unpack f) (unpack l)
jsonToPerson _          = Nothing
-}

jsonToPersonLens :: Value -> Maybe Person
jsonToPersonLens j = do String f <- j ^? key "first"
                        String l <- j ^? key "last"
                        return $ Person (unpack f) (unpack l)

jsonToClient :: FromJSON i => Value -> Parser (Client i)
jsonToClient (Object o) = 
  case M.lookup "type" o of
    Just (String "govorg")  -> GovOrg <$> o .: "id"  <*> o .: "name"
    Just (String "company") -> Company <$> o .: "id" <*> o .: "name"
                                       <*> o .: "person" <*> o .: "duty"
    Just (String "individual") -> Individual <$> o .: "id" <*> o .: "person"
    _                       -> Control.Applicative.empty
jsonToClient _ = Control.Applicative.empty

jsonToPerson :: Value -> Parser Person
jsonToPerson (Object o) = Person <$> o .: "first" <*> o .: "last"
jsonToPerson _          = Control.Applicative.empty

instance ToJSON (Client Integer) where
  toJSON = clientToJSON
instance ToJSON Person where
  toJSON = personToJSON

instance FromJSON i => FromJSON (Client i) where
  parseJSON = jsonToClient
instance FromJSON Person where
  parseJSON = jsonToPerson

saveClients :: FilePath -> [Client Integer] -> IO ()
saveClients fPath clients = runResourceT $
  yield (toJSON clients) $$ L.map (LB.toStrict . encode) =$ B.sinkFile fPath


