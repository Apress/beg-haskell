{-# LANGUAGE TransformListComp, RecordWildCards, ParallelListComp #-}

module Chapter3.Comprehensions where

import Chapter3.ParamPoly
import Chapter3.Lists

import Data.List
import Data.Char

import GHC.Exts

companyAnalytics :: [Client a] -> [(String, [(Person, String)])]
companyAnalytics clients = [ (the clientName, zip person duty)
                           | client@(Company { .. }) <- clients
                           , then sortWith by duty
                           , then group by clientName using groupWith
                           , then sortWith by length client
                           ]