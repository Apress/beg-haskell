{-# LANGUAGE LambdaCase #-}

module Chapter6.Lens where

import Control.Lens

data Client i = GovOrg     i String
              | Company    i String Person String
              | Individual i Person
              deriving Show

data Person = Person String String
              deriving Show

firstName :: Simple Lens Person String
firstName = lens (\(Person f _) -> f)
                 (\(Person _ l) newF -> Person newF l)

lastName :: Simple Lens Person String
lastName = lens (\(Person _ l) -> l)
                (\(Person f _) newL -> Person f newL)
                
identifier :: Lens (Client i) (Client j) i j
identifier = lens (\case (GovOrg i _)      -> i
                         (Company i _ _ _) -> i
                         (Individual i _)  -> i)
                  (\client newId -> case client of
                      GovOrg _ n      -> GovOrg newId n
                      Company _ n p r -> Company newId n p r
                      Individual _ p  -> Individual newId p)
                      
fullName' :: Simple Lens Person String
fullName' = lens (\(Person f l) -> f ++ " " ++ l)
                 (\_ newFullName -> case words newFullName of
                                      f:l:_ -> Person f l
                                      _     -> error "Incorrect name")