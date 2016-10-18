module Chapter13.Numbers where

data Number = Zero | Succ Number
            deriving Show

one :: Number
one = Succ Zero
two :: Number
two = Succ one
three :: Number
three = Succ two
four :: Number
four = Succ three
five :: Number
five = Succ four

plus' :: Number -> Number -> Number
plus' Zero     y = y
plus' (Succ x) y = Succ (plus' x y)

max' :: Number -> Number -> Number
max' Zero y = y
max' x Zero = x
max' (Succ x) (Succ y) = Succ (max' x y)

