{-# LANGUAGE DeriveFunctor #-}

module Chapter16.FreeMonads where

import Control.Monad.Free
import Data.Char
import Data.List

newtype ClientId = ClientId Integer           deriving Show
data Client = Client { clientName :: String } deriving Show

data AdminOp r = GetClient  ClientId        (Client   -> r)
               | SaveClient ClientId Client r
               | NewClient  Client          (ClientId -> r)
               deriving Functor

type Admin = Free AdminOp

getClient :: ClientId -> Admin Client
getClient i = liftF $ GetClient i id

saveClient :: ClientId -> Client -> Admin ()
saveClient i c = liftF $ SaveClient i c ()

newClient :: Client -> Admin ClientId
newClient c = liftF $ NewClient c id

exampleAdmin :: String -> Admin String
exampleAdmin s = do i <- newClient $ Client s
                    n <- fmap clientName $ getClient i
                    return $ map toUpper n

runAdmin :: Admin a -> ([(Integer,Client)],a)
runAdmin m = runAdmin' m []
  where runAdmin' (Free (GetClient  (ClientId i) n))   l = 
          let Just c = lookup i l in runAdmin' (n c) l
        runAdmin' (Free (SaveClient (ClientId i) c n)) l =
          let l' = deleteBy (\(j,_) (k,_) -> j == k) (i, c) l
           in runAdmin' n $ (i,c):l'
        runAdmin' (Free (NewClient c n)) [] =
          runAdmin' (n $ ClientId 1) [(1,c)]
        runAdmin' (Free (NewClient c n)) l =
          let (i',_) = maximumBy (\(j,_) (k,_) -> compare j k) l
           in runAdmin' (n $ ClientId (i' + 1)) $ (i' + 1,c):l
        runAdmin' (Pure c)                  l = (l, c)
