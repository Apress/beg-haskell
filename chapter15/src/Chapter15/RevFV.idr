module Chapter15.RevFV

rev : List a -> List a
rev []      = []
rev (x::xs) = rev xs ++ [x]

appendLemma : (x : a) -> (xs : List a) -> (ys : List a) -> ((x::xs) ++ ys = x::(xs ++ ys))
appendLemma x []      ys = refl
appendLemma x (z::zs) ys = refl

revProp1 : (xs : List a) -> (ys : List a) -> (rev (xs ++ ys) = rev ys ++ rev xs)
revProp1 []      ys = ?revProp1Empty
revProp1 (x::xs) ys = ?revProp1Induction

Chapter15.RevFV.revProp1Empty = proof
  intros
  rewrite sym (appendNilRightNeutral (rev ys))
  trivial

{-
mutual
  lemmaIdempotent : (x : a) -> (xs : List a) -> (rev(xs ++ [x]) = x::rev xs)
  lemmaIdempotent x []      = refl
  lemmaIdempotent x (y::ys) = let i = revIdempotent (y::ys) in ?lemmaIdempotentStep
  
  revIdempotent : (lst : List a) -> (rev (rev lst) = lst)
  revIdempotent []      = refl
  revIdempotent (x::xs) = let i = lemmaIdempotent x xs in ?revIdempotentStep
-}
