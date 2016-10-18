module Chapter6.STRef where

import Control.Monad.ST
import Data.STRef

listLength :: [a] -> Integer
listLength list = runST $ do l <- newSTRef 0
                             traverseList list l
                             readSTRef l
                  where traverseList []     _ = return ()
                        traverseList (_:xs) l = do modifySTRef' l (+1)
                                                   traverseList xs l
