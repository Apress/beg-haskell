{-# LANGUAGE DeriveGeneric #-}

module Chapter8.APriori.Types where

import Data.Set (Set)
import qualified Data.Set as S

import Control.DeepSeq
import GHC.Generics

data Client = GovOrg     { clientName :: String }
            | Company    { clientName :: String, person :: Person, duty :: String }
            | Individual { person :: Person }
            deriving (Show, Eq, Ord, Generic)
            
data ClientKind = KindGovOrg | KindCompany | KindIndividual
                deriving (Show, Eq, Ord, Generic)
              
data Person = Person { firstName :: String, lastName :: String, gender :: Gender }
            deriving (Show, Eq, Ord, Generic)

data Gender = Male | Female | UnknownGender
            deriving (Show, Eq, Ord, Generic)
              
data Product = Product { productId :: Integer, productType :: ProductType }
             deriving (Show, Eq, Ord, Generic)
               
data ProductType = TimeMachine | TravelGuide | Tool | Trip
                 deriving (Show, Eq, Ord, Generic)
                   
data Purchase = Purchase { client :: Client, products :: [Product] }
              deriving (Show, Eq, Ord, Generic)
                
data PurchaseInfo = InfoClientKind           ClientKind
                  | InfoClientDuty           String
                  | InfoClientGender         Gender
                  | InfoPurchasedProduct     Integer
                  | InfoPurchasedProductType ProductType
                  deriving (Show, Eq, Ord, Generic)

instance NFData Client
instance NFData ClientKind
instance NFData Person
instance NFData Gender
instance NFData Product
instance NFData ProductType
instance NFData Purchase
instance NFData PurchaseInfo

clientToPurchaseInfo :: Client -> Set PurchaseInfo
clientToPurchaseInfo GovOrg { } =
  S.singleton $ InfoClientKind KindGovOrg
clientToPurchaseInfo Company { duty = d } =
  S.fromList [ InfoClientKind KindCompany, InfoClientDuty d ]
clientToPurchaseInfo Individual { person = Person { gender = UnknownGender } } =
  S.singleton $ InfoClientKind KindIndividual 
clientToPurchaseInfo Individual { person = Person { gender = g } } =
  S.fromList [ InfoClientKind KindIndividual, InfoClientGender g ]

productsToPurchaseInfo :: [Product] -> Set PurchaseInfo
productsToPurchaseInfo = foldr
  (\(Product i t) pinfos -> S.insert (InfoPurchasedProduct i) $
                            S.insert (InfoPurchasedProductType t) pinfos)
  S.empty
  
purchaseToTransaction :: Purchase -> Transaction
purchaseToTransaction (Purchase c p) =
  Transaction $ clientToPurchaseInfo c `S.union` productsToPurchaseInfo p
  
newtype Transaction = Transaction (Set PurchaseInfo) deriving (Eq, Ord, Generic)
newtype FrequentSet = FrequentSet (Set PurchaseInfo) deriving (Eq, Ord, Generic)
data AssocRule = AssocRule (Set PurchaseInfo) (Set PurchaseInfo) deriving (Eq, Ord, Generic)

instance NFData Transaction
instance NFData FrequentSet
instance NFData AssocRule

instance Show AssocRule where
  show (AssocRule a b) = show a ++ " => " ++ show b

setSupport :: [Transaction] -> FrequentSet -> Double
setSupport transactions (FrequentSet sElts) =
  let total = length transactions
      supp  = length (filter (\(Transaction tElts) -> sElts `S.isSubsetOf` tElts) transactions)
   in fromIntegral supp / fromIntegral total
   
ruleConfidence :: [Transaction] -> AssocRule -> Double
ruleConfidence transactions (AssocRule a b) =
  setSupport transactions (FrequentSet $ a `S.union` b) / setSupport transactions (FrequentSet a)
