{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies,
             ScopedTypeVariables, OverloadedStrings #-}

module Main where

import Yesod
import qualified Text.Blaze.Html5 as H

import Database.Persist.Sqlite
import Chapter12.Database

data TimeMachineStore = TimeMachineStore ConnectionPool
instance Yesod TimeMachineStore

instance YesodPersist TimeMachineStore where
  type YesodPersistBackend TimeMachineStore = SqlPersistT

  runDB action = do TimeMachineStore pool <- getYesod
                    runSqlPool action pool

mkYesod "TimeMachineStore" [parseRoutes|
/about             AboutR   GET
/about1            About1R  GET
/about2            About2R  GET
/client/#ClientId  ClientR  GET
/client2/#ClientId Client2R GET
/is1984/#Integer   Is1984R  GET
|]

getAboutR :: Handler Html
getAboutR = return $ H.html $ H.body $
                       H.p $ H.toHtml ("Hello Beginning Haskell!" :: String)

getAbout1R :: Handler Html
getAbout1R = return [shamlet|
<html>
  <body>
    <p>Hello Beginning Haskell!
|]

getAbout2R :: Handler Html
getAbout2R = defaultLayout [whamlet|Hello Beginning Haskell!|]

getClientR :: ClientId -> Handler TypedContent
getClientR clientId = do
  c <- runDB $ get404 clientId
  selectRep $ do
    provideRep $ defaultLayout [whamlet|
      <h1>#{clientFirstName c} #{clientLastName c}
      <p>#{clientAddress c}
      $maybe a <- clientAge c
        <p>#{a} years old
      $nothing
        <p>Unknown age
    |]
    provideRep $ returnJson $ c

getClient2R :: ClientId -> Handler Html
getClient2R clientId = do
  c <- runDB $ get404 clientId
  defaultLayout [whamlet|
    <h1>#{clientFirstName c} #{clientLastName c}
    <p>#{clientAddress c}
    $maybe a <- clientAge c
      <p>#{a} years old
    $nothing
      <p>Unknown age
  |]

getIs1984R :: Integer -> Handler Html
getIs1984R 1984 = defaultLayout [whamlet|
  <h1>Yes!
  <ul>
    <li><a href=@{AboutR}>About us
    <li><a href=@{Is1984R 1983}>Is is 1983?
|]
getIs1984R _    = defaultLayout [whamlet|<h1>No!|]

-- main :: IO ()
-- main = warp 3000 TimeMachineStore

main :: IO ()
main = withSqlitePool "example.db" 3 $ \pool ->
  warp 3000 $ TimeMachineStore pool
