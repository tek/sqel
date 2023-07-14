module Sqel.Kind.List where

type AppendList :: [k] -> [k] -> [k]
type family AppendList l r where
  AppendList '[] '[] = '[]
  AppendList l '[] = l
  AppendList (l0 : l) r = l0 : AppendList l r
  AppendList '[] r = r

-- | Append two type lists or @Symbol@s.
type (++) :: ∀ k . k -> k -> k
type family (++) l r where
  (++) @[_] l r = AppendList l r
  (++) @Symbol l r = AppendSymbol l r

infixr 5 ++
