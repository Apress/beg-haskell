{-# LANGUAGE OverloadedStrings #-}

module Chapter10.Builder where

import Data.Conduit
import qualified Data.Conduit.Binary as B
import qualified Data.Conduit.List as L
import qualified Data.Conduit.Text as T
import Data.Monoid
import Data.Text.Lazy
import Data.Text as T
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B

import Chapter10.Types

saveClients :: FilePath -> [Client Int] -> IO ()
saveClients fpath clients = runResourceT $
  L.sourceList clients $$ L.map clientToText' =$= L.map (toStrict . B.toLazyText)
    =$= L.concatMap (\x -> [x, "\n"])  -- write '\n' between clients
    =$= T.encode T.utf8 =$ B.sinkFile fpath

clientToText :: Client Int -> T.Text
clientToText (GovOrg  i n)     =
  "client(gov," <> escapeString (show i) <> "," <> escapeString n <> ")"
clientToText (Company i n p d) = 
  "client(com," <> escapeString (show i) <> "," <> escapeString n <> ","
    <> personToText p <> "," <> escapeString d <> ")"
clientToText (Individual i p)  =
  "client(ind," <> escapeString (show i) <> "," <> personToText p <> ")"

personToText :: Person -> T.Text
personToText (Person f l) = "person(" <> escapeString f <> "," <> escapeString l <> ")"

escapeString :: String -> T.Text
escapeString = T.replace "\n" "\\n" . T.replace ","  "\\," .
               T.replace "("  "\\(" . T.replace ")"  "\\(" . T.pack

clientToText' :: Client Int -> B.Builder
clientToText' (GovOrg i n) =
  "client(gov," <> B.decimal i <> B.singleton ',' 
                <> B.fromText (escapeString n) <> B.singleton ')'
clientToText' (Company i n p d) =
  "client(com," <> B.decimal i <> B.singleton ',' 
                <> B.fromText (escapeString n) <> B.singleton ','
                <> personToText' p <> B.singleton ',' 
                <> B.fromText (escapeString d) <> B.singleton ')'
clientToText' (Individual i p) = 
  "client(ind," <> B.decimal i <> B.singleton ',' 
                <> personToText' p <> B.singleton ')'

personToText' :: Person -> B.Builder
personToText' (Person f l) =
  "person(" <> B.fromText (escapeString f) <> B.singleton ',' 
            <> B.fromText (escapeString l) <> B.singleton ')'

