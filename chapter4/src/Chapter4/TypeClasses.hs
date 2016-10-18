module Chapter4.TypeClasses where

class Nameable n where
  name :: n -> String

initial :: Nameable n => n -> Char
initial n = head (name n)

data Client i = GovOrg  { clientId :: i, clientName :: String }
              | Company { clientId :: i, clientName :: String
                         , person :: Person, duty :: String }
              | Individual { clientId :: i, person :: Person }
              deriving Show

data Person = Person { firstName :: String, lastName  :: String }
              deriving (Show, Read)

instance Nameable (Client i) where
  name Individual { person = Person { firstName = f, lastName = n } }
         = f ++ " " ++ n
  name c = clientName c

data Complex = C Double Double deriving (Show, Eq)

instance Num Complex where
  (C a1 b1) + (C a2 b2) = C (a1 + a2) (b1 + b2)
  (C a1 b1) - (C a2 b2) = C (a1 - a2) (b1 - b2)
  (C a1 b1) * (C a2 b2) = C (a1*a2-b1*b2) (a1*b2+b1*a2)
  negate (C a b)        = C (negate a) (negate b)
  fromInteger n         = C (fromInteger n) 0
  abs (C a b)           = C (sqrt $ a*a+b*b) 0
  signum c@(C a b)      = let C n _ = abs c in C (a / n) (b / n)