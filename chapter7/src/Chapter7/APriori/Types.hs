module Chapter7.APriori.Types where

import Data.Set (Set)
import qualified Data.Set as S

data Client = GovOrg     { clientName :: String }
            | Company    { clientName :: String, person :: Person, duty :: String }
            | Individual { person :: Person }
            deriving (Show, Eq, Ord)
            
data ClientKind = KindGovOrg | KindCompany | KindIndividual
                deriving (Show, Eq, Ord)
              
data Person = Person { firstName :: String, lastName :: String, gender :: Gender }
            deriving (Show, Eq, Ord)

data Gender = Male | Female | UnknownGender
            deriving (Show, Eq, Ord)
              
data Product = Product { productId :: Integer, productType :: ProductType }
             deriving (Show, Eq, Ord)
               
data ProductType = TimeMachine | TravelGuide | Tool | Trip
                 deriving (Show, Eq, Ord)
                   
data Purchase = Purchase { client :: Client, products :: [Product] }
              deriving (Show, Eq, Ord)
                
data PurchaseInfo = InfoClientKind           ClientKind
                  | InfoClientDuty           String
                  | InfoClientGender         Gender
                  | InfoPurchasedProduct     Integer
                  | InfoPurchasedProductType ProductType
                  deriving (Show, Eq, Ord)

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
  
newtype Transaction = Transaction (Set PurchaseInfo) deriving (Eq, Ord)
newtype FrequentSet = FrequentSet (Set PurchaseInfo) deriving (Eq, Ord)
data AssocRule = AssocRule (Set PurchaseInfo) (Set PurchaseInfo) deriving (Eq, Ord)

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
