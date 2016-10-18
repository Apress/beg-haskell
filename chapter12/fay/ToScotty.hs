{-# LANGUAGE OverloadedStrings, RebindableSyntax #-}

module ToScotty where

import Fay.Text
import FFI
import JQuery
import Prelude hiding (concat)

data Product = Product { name :: Text
                       , description :: Text
                       , price :: Double
                       , inStock :: Int
                       }

main :: Fay ()
main = ready $ do
  productButton <- select "#productButton"
  onClick onProductButtonClick productButton
  return ()

onProductButtonClick :: Event -> Fay Bool
onProductButtonClick _ = do
  productField <- select "#productId"
  productId <- getVal productField
  let url = concat ["http://localhost:3000/json/product/", productId]
  ajax url
       (\p -> alert $ name p)
       (\_ _ _ -> alert "Error in AJAX request")
  return True
 
alert :: Text -> Fay ()
alert = ffi "alert(%1)"
