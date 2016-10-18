module Chapter13.ListL

data ListL : Nat -> Type -> Type where
  EmptyL : ListL Z a
  ConsL  : a -> ListL n a -> ListL (S n) a

duplicateEachElem : ListL n a -> ListL (n * 2) a
duplicateEachElem EmptyL      = EmptyL
duplicateEachElem (ConsL x r) = ConsL x (ConsL x (duplicateEachElem r))

{-
reverseAccum : ListL p a -> ListL q a -> ListL (q <+> p) a
reverseAccum EmptyL      acc = acc
reverseAccum (ConsL x r) acc = reverseAccum r (ConsL x acc)

reverseL : ListL n a -> ListL n a
reverseL l = reverseAccum l EmptyL
-}

