{-# LANGUAGE FlexibleContexts, DeriveGeneric #-}

module Chapter9.Types where

import Control.Monad.Error
import Data.Binary
import GHC.Generics

data Client i = GovOrg  { clientId :: i, clientName :: String }
              | Company { clientId :: i, clientName :: String
                         , person :: Person, duty :: String }
              | Individual { clientId :: i, person :: Person }
              deriving Show

data Person = Person { firstName :: String, lastName  :: String }
              deriving (Show, Read, Generic)

instance Binary Person
              
data CompanyNameError = GovOrgArgument | IndividualArgument

companyName :: MonadError CompanyNameError m => Client i -> m String
companyName Company { clientName = n } = return n
companyName GovOrg     { }             = throwError GovOrgArgument
companyName Individual { }             = throwError IndividualArgument

companyNameDef :: MonadError CompanyNameError m => Client i -> m String
companyNameDef c = companyName c `catchError` (\_ -> return "")
