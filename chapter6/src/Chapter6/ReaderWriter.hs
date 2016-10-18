-- {-# LANGUAGE TemplateHaskell #-}

module Chapter6.ReaderWriter where

import Control.Monad.Reader
import Control.Monad.Writer
-- import Control.Lens

import Data.Char

import Chapter6.Lens2
import Chapter6.Vector
import Chapter6.KMeans

data Settings e v = Settings { i :: Int -> [e] -> [v], k :: Int, th :: Double, user :: Person }

kMeansMain :: (Vector v, Vectorizable e v) => [e] -> Reader (Settings e v) [v]
kMeansMain points = do i' <- asks i
                       k' <- asks k
                       t' <- asks th
                       return $ kMeans i' k' points t'

saveKMeans :: Vector v => [v] -> Reader (Settings e v) ()
saveKMeans centroids =
  do u <- asks user
     printString $ "Saving for user: " ++ show u
     local (\s -> let Person f l = user s
                   in s { user = Person (map toUpper f) l }) $
           do u' <- asks user
              saveDatabase centroids u'
     return ()

compareClusters :: (Vector v, Vectorizable e v) => [e] -> Reader (Settings e v) ([v], [v])
compareClusters points = do c1 <- kMeansMain points
                            c2 <- local (\s -> s { k = k s + 1 })
                                        (kMeansMain points)
                            return (c1, c2)

printString :: String -> Reader (Settings e v) ()
printString _ = return ()

saveDatabase :: Vector v => [v] -> Person -> Reader (Settings e v) ()
saveDatabase _ _ = return ()

readInformation :: Writer String [String]
readInformation = return []

computeValue :: [String] -> Writer String ()
computeValue _ = return ()

accessDatabase :: Writer String ()
accessDatabase = do tell "Start database access"
                    info <- readInformation
                    computeValue info
                    tell "Finish database access"
