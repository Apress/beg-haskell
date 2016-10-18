module Main where

import Options.Applicative

data Args = Args String Bool  -- data type holding the arguments
          deriving Show

args :: Parser Args           -- read the arguments
args = Args <$> strOption (long "file" <> help "Database of clients to load")
            <*> switch    (long "json" <> help "Whether the database uses JSON") 

argsInfo :: ParserInfo Args   -- define arguments + help text
argsInfo = info args fullDesc

main :: IO ()
main = do Args fPath json <- execParser argsInfo
          print (fPath, json)
