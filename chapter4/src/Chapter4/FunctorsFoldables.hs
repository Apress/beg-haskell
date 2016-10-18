module Chapter4.FunctorsFoldables where

import Chapter4.MinimumPrice

import qualified Data.Map as M
import qualified Data.Tree as T

modifyTravelGuidePrice :: Double -> [TravelGuide] -> [TravelGuide]
modifyTravelGuidePrice m  = map (\tg -> tg { price = m * price tg })

modifyTravelGuidePriceMap :: Double -> M.Map a TravelGuide -> M.Map a TravelGuide
modifyTravelGuidePriceMap m = M.map (\tg -> tg { price = m * price tg })

modifyTravelGuidePriceTree :: Double -> T.Tree TravelGuide -> T.Tree TravelGuide
modifyTravelGuidePriceTree m = fmap (\tg -> tg { price = m * price tg })

modifyTravelGuidePrice' :: Functor f => Double -> f TravelGuide -> f TravelGuide
modifyTravelGuidePrice' m  = fmap (\tg -> tg { price = m * price tg })

{-
instance Functor ((->) r) where
  fmap f g = f . g
-}