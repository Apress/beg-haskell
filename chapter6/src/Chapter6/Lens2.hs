{-# LANGUAGE TemplateHaskell #-}

module Chapter6.Lens2 where

import Control.Lens
-- import Control.Lens.Cons

-- import Data.Char

data Client i = GovOrg     { _identifier :: i, _name :: String }
              | Company    { _identifier :: i, _name :: String
                           , _person :: Person, _duty :: String }
              | Individual { _identifier :: i, _person :: Person }
              deriving Show
              
data Person = Person { _firstName :: String, _lastName :: String }
              deriving Show
              
makeLenses ''Client
makeLenses ''Person
                      
fullName :: Simple Lens Person String
fullName = lens (\(Person f l) -> f ++ " " ++ l)
                (\_ newFullName -> case words newFullName of
                                     f:l:_ -> Person f l
                                     _     -> error "Incorrect name")