{-# LANGUAGE OverloadedLiterals #-}
module Chapter13.DatabaseSpliced where

import qualified Data.Map as M
import Database.Persist
import Database.Persist.TH

type Product = ProductGeneric SqlBackend
data ProductGeneric backend = Product { productName :: !String
                                      , productDescription :: !String
                                      , productPrice :: !Double
                                      , productInStock :: !Int}
                              deriving Show
type ProductId = KeyBackend SqlBackend Product

instance PersistEntity (ProductGeneric backend) where
  data instance Unique (ProductGeneric backend)
  data instance EntityField (ProductGeneric backend) typ where
    ProductId          :: EntityField (ProductGeneric backend) ProductId
    ProductName        :: EntityField (ProductGeneric backend) String
    ProductDescription :: EntityField (ProductGeneric backend) String
    ProductPrice       :: EntityField (ProductGeneric backend) Double
    ProductInStock     :: EntityField (ProductGeneric backend) Int
  type instance PersistEntityBackend (ProductGeneric backend) = backend
  
  entityDef _ = EntityDef (HaskellName "Product") (DBName "product") (DBName "id") []
    [ FieldDef (HaskellName "name") (DBName "name") (FTTypeCon "String") SqlString [] True Nothing
    , FieldDef (HaskellName "description") (DBName "description") (FTTypeCon "String") SqlString [] True Nothing
    , FieldDef (HaskellName "price") (DBName "price") (FTTypeCon "Double") SqlReal [] True Nothing
    , FieldDef (HaskellName "inStock") (DBName "in_stock") (FTTypeCon "Int") SqlInt64 [] True Nothing ]
    [] ["Show"] (M.fromList []) False
  persistFieldDef ProductId = ...
  persistFieldDef ...
  toPersistFields (Product n d p i) =
    [SomePersistField n, SomePersistField d, SomePersistField p, SomePersistField i]
  fromPersistValues [n, d, p, i] = ...
  persistUniqueToFieldNames _ = ...
  persistUniqueToValues _ = ...
  persistUniqueKeys (Product n d p i) = []
  persistIdField = ProductId
  fieldLens ProductId = ...
  fieldLens ...
