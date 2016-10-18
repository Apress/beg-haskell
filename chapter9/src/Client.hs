module Main where

import Control.Monad.Trans
import qualified Data.ByteString.Char8 as BS
import Data.Conduit
import Data.Conduit.Network
import System.Environment
import Network

main :: IO ()
main = withSocketsDo $ do
         (name:_) <- getArgs
         runTCPClient (clientSettings 8900 (BS.pack "127.0.0.1")) (clientApp name)

clientApp :: String -> Application IO
clientApp name d = do (yield $ BS.pack name) $$ appSink d
                      appSource d $$ (do Just w <- await
                                         lift $ BS.putStrLn w)
