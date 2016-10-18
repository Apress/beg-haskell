{-# LANGUAGE TemplateHaskell #-}

module Chapter6.StateLenses where

import Chapter6.Lens2

import Control.Lens
import Control.Monad.State
import Data.Char

data ExampleState = ExampleState { _increment :: Int, _clients :: [Client Int] }
                  deriving Show
makeLenses ''ExampleState

zoomExample :: State ExampleState ()
zoomExample = do n <- use increment
                 zoom (clients.traversed) $ do
                   identifier      += n
                   person.fullName %= map toUpper
