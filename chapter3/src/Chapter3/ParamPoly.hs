module Chapter3.ParamPoly where

maybeString :: Maybe t -> String
maybeString (Just _) = "Just"
maybeString Nothing  = "Nothing"

swapTriple (x,y,z) = (y,z,x)

duplicate x = (x,x)

nothing _ = Nothing

index []     = []
index [x]    = [(0,x)]
index (x:xs) = let indexed@((n,_):_) = index xs
               in  (n+1,x):indexed

maybeA [] = 'a'

data Client i = GovOrg  { clientId :: i, clientName :: String }
              | Company { clientId :: i, clientName :: String
                         , person :: Person, duty :: String }
              | Individual { clientId :: i, person :: Person }
              deriving Show

data Person = Person { firstName :: String, lastName  :: String }
              deriving Show

data Triple a b c = Triple a b c

data SamePair a = SamePair a a