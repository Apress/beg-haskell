module Chapter5.Annotations where

import Control.DeepSeq

data Client = GovOrg     {-# UNPACK #-} !Int String
            | Company    {-# UNPACK #-} !Int String Person String
            | Individual {-# UNPACK #-} !Int Person
            deriving Show
            
instance NFData Client where
  rnf (GovOrg i n)                 = i `deepseq` n `deepseq` ()
  rnf (Company i n (Person f l) r) = i `deepseq` n `deepseq` f `deepseq` l
                                       `deepseq` r `deepseq` ()
  rnf (Individual i (Person f l))  = i `deepseq` f `deepseq` l `deepseq` ()

data Person = Person { firstName :: String, lastName  :: String }
              deriving Show