module Chapter6.IncompleteData where

import Data.Maybe

meanPurchase :: Integer -- the client identifier
             -> Double  -- the mean purchase
meanPurchase clientId = let p = purchasesByClientId clientId
                         in foldr (+) 0.0 $ catMaybes $ map purchaseValue p

thenDo :: Maybe a -> (a -> Maybe b) -> Maybe b
thenDo Nothing  _ = Nothing
thenDo (Just x) f = f x

{-
purchaseValue :: Integer -> Maybe Double
purchaseValue purchaseId =
  case numberItemsByPurchaseId purchaseId of
    Nothing -> Nothing
    Just n  -> case productIdByPurchaseId purchaseId of
                 Nothing        -> Nothing
                 Just productId -> case priceByProductId productId of
                                     Nothing    -> Nothing
                                     Just price -> Just $ (fromInteger n) * price
-}

purchaseValue :: Integer -> Maybe Double
purchaseValue purchaseId =
  numberItemsByPurchaseId purchaseId `thenDo` (\n ->
  productIdByPurchaseId purchaseId   `thenDo` (\productId ->
  priceByProductId productId         `thenDo` (\price ->
  Just $ fromInteger n * price       )))


purchasesByClientId :: Integer -> [Integer]
purchasesByClientId = error "Unimplemented"

numberItemsByPurchaseId :: Integer -> Maybe Integer
numberItemsByPurchaseId = error "Unimplemented"

productIdByPurchaseId :: Integer -> Maybe Integer
productIdByPurchaseId = error "Unimplemented"

priceByProductId :: Integer -> Maybe Double
priceByProductId = error "Unimplemented"

purchaseValueWithDo :: Integer -> Maybe Double
purchaseValueWithDo purchaseId = do n         <- numberItemsByPurchaseId purchaseId
                                    productId <- productIdByPurchaseId purchaseId
                                    price     <- priceByProductId productId
                                    return $ fromInteger n * price
