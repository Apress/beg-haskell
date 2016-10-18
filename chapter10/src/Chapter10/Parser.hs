{-# LANGUAGE OverloadedStrings #-}

module Chapter10.Parser where

import Control.Applicative
import Data.Text
import Data.Attoparsec.Text

import Data.Conduit
import Data.Conduit.Attoparsec
import qualified Data.Conduit.Binary as B
import qualified Data.Conduit.Text as T

import Chapter10.Types

data GreetingYear = GreetingYear Text Int
data GreetingYear' = GreetingYear' Greeting Int

data Greeting = Hello | Bye

greetingYearParser :: Parser GreetingYear
greetingYearParser = GreetingYear <$> (string "hello" <|> string "bye") <*> decimal

greetingYearParserSpace :: Parser GreetingYear
greetingYearParserSpace = (\g _ y -> GreetingYear g y) <$>
                            (string "hello" <|> string "bye") <*> char ' ' <*> decimal

greetingYearParserSpace' :: Parser GreetingYear
greetingYearParserSpace' = GreetingYear <$> (string "hello" <|> string "bye") <* char ' ' <*> decimal


aChar :: Parser Char
aChar =     (const ',')  <$> (string "\\,")
        <|> (const '\n') <$> (string "\\n")
        <|> (const '(')  <$> (string "\\(")
        <|> (const ')')  <$> (string "\\)")
        <|> satisfy (notInClass ",\n()")

aString' :: Parser String
aString' = ((:) <$> aChar <*> aString') <|> (pure "")

aString :: Parser String
aString = many aChar

aPerson :: Parser Person
aPerson = Person <$ string "person(" <*> aString <* char ',' <*> aString <* char ')'

aClient :: Parser (Client Int)
aClient =     GovOrg     <$ string "client(gov," <*> (decimal <?> "Only integer ids")
                         <* char ',' <*> aString <* char ')'
          <|> Company    <$ string "client(com," <*> decimal 
                         <* char ',' <*> aString <* char ','
                         <*> aPerson <* char ',' <*> aString <* char ')'
          <|> Individual <$ string "client(ind," <*> decimal 
                         <* char ',' <*> aPerson <* char ')'

parseClients :: Parser [Client Int]
parseClients = sepBy aClient (char '\n')

loadClients :: FilePath -> IO [Client Int]
loadClients fPath = runResourceT $
  B.sourceFile fPath $$ T.decode T.utf8 =$ sinkParser parseClients

