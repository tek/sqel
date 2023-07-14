module Sqel.Kind.List where

-- TODO try to optimize by matching on up to 10 or so elements explicitly?
type AppendList :: [k] -> [k] -> [k]
type family AppendList l r where
  AppendList '[] '[] = '[]
  AppendList l '[] = l
  AppendList '[] r = r
  AppendList (l0 : l) r = l0 : AppendList l r

-- | Append two type lists or @Symbol@s.
type (++) :: ∀ k . k -> k -> k
type family (++) l r where
  (++) @[_] l r = AppendList l r
  (++) @Symbol l r = AppendSymbol l r

infixr 5 ++
