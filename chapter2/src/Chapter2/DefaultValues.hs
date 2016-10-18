module Chapter2.DefaultValues where

-- Information for a connection
-- * URL to connect to
-- * Connection type: TCP or UDP
-- * Connection speed
-- * Whether to use a proxy
-- * Whether to use caching
-- * Whether to use keep-alive
-- * Time out
data ConnType = TCP | UDP
data UseProxy = NoProxy | Proxy String
data TimeOut = NoTimeOut | TimeOut Integer

data Connection = Connection -- Just a placeholder
                  deriving Show

connect :: String -> ConnType -> Integer -> UseProxy -> Bool -> Bool -> TimeOut -> Connection
connect _ _ _ _ _ _ _ = undefined

connectUrl :: String -> Connection
connectUrl u = connect u TCP 0 NoProxy False False NoTimeOut

data ConnOptions = ConnOptions { connType      :: ConnType
                               , connSpeed     :: Integer
                               , connProxy     :: UseProxy
                               , connCaching   :: Bool
                               , connKeepAlive :: Bool
                               , connTimeOut   :: TimeOut
                               }

connect' :: String -> ConnOptions -> Connection
connect' _ _ = undefined

connDefault :: ConnOptions
connDefault = ConnOptions TCP 0 NoProxy False False NoTimeOut