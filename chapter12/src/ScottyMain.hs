{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards,
             QuasiQuotes, TemplateHaskell #-}

module Main where

import Prelude hiding (product)

import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mconcat)
import qualified Database.Persist.Sqlite as Db
import Network.HTTP.Types
import Data.Text.Lazy
import Text.Hamlet
import Web.Scotty

import Control.Applicative ((<$>), (<*>))
import Text.Digestive
import Text.Digestive.Blaze.Html5
-- import Text.Digestive.Aeson
import Text.Digestive.Util

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Chapter12.Database

main :: IO()
main = do
  Db.runSqlite "example.db" $ Db.runMigration migrateAll
  -- liftIO $ Db.runSqlite "example.db" insertInitialData
  
  Db.withSqlitePool "example.db" 10 $ \pool ->
    scotty 3000 $ do
      get "/about" $ 
        html $ mconcat [ "<html><body>"
                       , "  <h1>Hello Beginning Haskell!</h1>"
                       , "</body></html>" ]
      
      get "/product/:productId" $ do
        (productId :: Integer) <- param "productId"
        product <- liftIO $ flip Db.runSqlPersistMPool pool $
                     Db.get $ Db.Key (Db.PersistInt64 $ fromIntegral productId)
        case product of
          Just (Product { .. }) -> html $ mconcat [ "<html><body>"
                                                  , "  <h1>"
                                                  , pack productName
                                                  , "  </h1>"
                                                  , "  <p>"
                                                  , pack productDescription
                                                  , "  </p>"
                                                  , "</html></body>" ]
          Nothing               -> status notFound404
      
      get "/product2/:productId" $ do
        (productId :: Integer) <- param "productId"
        product <- liftIO $ flip Db.runSqlPersistMPool pool $
                     Db.get $ Db.Key (Db.PersistInt64 $ fromIntegral productId)
        case product of
          Just (Product { .. }) -> html $ renderHtml $
            H.html $ do
              H.head $ 
                H.title "Time Machine Store"
              H.body $ do
                H.h1 $ H.toHtml productName
                H.p  H.! A.id "descr" $ H.toHtml productDescription
          Nothing               -> status notFound404

      get "/product3/:productId" $ do
        (productId :: Integer) <- param "productId"
        product <- liftIO $ flip Db.runSqlPersistMPool pool $
                     Db.get $ Db.Key (Db.PersistInt64 $ fromIntegral productId)
        case product of
          Just p -> html $ renderHtml [shamlet|
            <html>
              <head>
                <title>Time Machine Store
              <body>
                <h1>#{productName p}
                <p id=descr>#{productDescription p}
            |]
          Nothing               -> status notFound404
      
      get "/json/product/:productId" $ do
        (productId :: Integer) <- param "productId"
        (product :: Maybe Product) <-
          liftIO $ flip Db.runSqlPersistMPool pool $
            Db.get $ Db.Key (Db.PersistInt64 $ fromIntegral productId)
        case product of
          Just p  -> do setHeader "Access-Control-Allow-Origin" "*"
                        json p
          Nothing -> status notFound404
      
      get "/products" $ do
        (products :: [Db.Entity Product]) <-
          liftIO $ flip Db.runSqlPersistMPool pool $ Db.selectList [] []
        html $ renderHtml [shamlet|
          <html>
            <body>
            <h1>Products
            <table>
              <tr>
                <th>Name
                <th>Description
              $forall Db.Entity _ p <- products
                <tr>
                  <td>#{productName p}
                  <td>#{productDescription p}
        |]
        
      get "/new-product" $ do
        view <- getForm "product" productForm
        let view' = fmap H.toHtml view
        html $ renderHtml $
          H.html $ do
            H.head $ H.title "Time Machine Store"
            H.body $ productView view' 
      
      post "/new-product" $ do
        params' <- params
        (view,product) <- postForm "product" productForm (paramsToEnv params')
        case product of
          Just p -> do
            Db.Key (Db.PersistInt64 newId) <- liftIO $ flip Db.runSqlPersistMPool pool $ Db.insert p
            redirect $ mconcat ["/product/", pack $ show newId]
          Nothing -> do
            let view' = fmap H.toHtml view
            html $ renderHtml $
              H.html $ do
                H.head $ H.title "Time Machine Store"
                H.body $ productView view'
      
      notFound $ do
        status notFound404
        html $ "<h1>Not found :(</h1>"

countryForm :: Monad m => Form String m Country
countryForm = Country <$> "name" .: string Nothing
                      <*> "send" .: bool (Just True)

productForm :: Monad m => Form String m Product
productForm = Product <$> "name"        .: string Nothing
                      <*> "description" .: string Nothing
                      <*> "price"       .: validate isANumber (string Nothing)
                      <*> "inStock"     .: check "Must be >= 0" (>= 0)
                                             (validate isANumber (string Nothing))
                      
productView :: View H.Html -> H.Html
productView view = do
  form view "/new-product" $ do
    label     "name"    view "Name:"
    inputText "name"    view
    H.br
    inputTextArea Nothing Nothing "description" view
    H.br
    label     "price"   view "Price:"
    inputText "price"   view
    errorList "price"   view
    label     "inStock" view "# in Stock:"
    inputText "inStock" view
    errorList "inStock" view
    H.br
    inputSubmit "Submit"

isANumber :: (Num a, Read a) => String -> Result String a
isANumber = maybe (Error "Not a number") Success . readMaybe

paramsToEnv :: Monad m => [Param] -> Text.Digestive.Env m
paramsToEnv []           _ = fail "Parameter not found"
paramsToEnv ((k,v):rest) t = if toStrict k == fromPath t
                               then return [TextInput $ toStrict v]
                               else paramsToEnv rest t

insertInitialData :: Db.PersistStore m => m ()
insertInitialData = do
  spain   <- Db.insert $ Country "Spain" True
  germany <- Db.insert $ Country "Germany" True
  uk      <- Db.insert $ Country "United Kingdom" False
  _usa    <- Db.insert $ Country "United States of America" False
  
  client1  <- Db.insert $ Client "Alejandro" "Serrano" "Home Town, 1" spain Nothing
  client2  <- Db.insert $ Client "Werner" "Heisenberg" "A place in Wurzburg" germany (Just 50)
  _client3 <- Db.insert $ Client "Doctor" "Who" "Police Box" uk Nothing
  
  let longDescription = "blah blah blah"
  product1 <- Db.insert $ Product "Travel to the XIX Century" longDescription 12.3 3
  product2 <- Db.insert $ Product "TM-223 Machine" longDescription 1245.0 10
  
  _ <- Db.insert $ Purchase client1 product1 1 20.0
  _ <- Db.insert $ Purchase client1 product2 5 1002.3
  _ <- Db.insert $ Purchase client2 product1 3 58.0
  
  return ()
