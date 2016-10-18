{-# LANGUAGE GADTs,DataKinds, TypeOperators #-}

module Chapter13.VectorsLits where

import GHC.TypeLits

data Vect n a where
  VNil  :: Vect 0 a
  VCons :: a -> Vect n a -> Vect (n + 1) a

