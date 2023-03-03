-- | Description: Type functions
module Sqel.Kind where

-- | Append two type lists.
type family (++) l r where
  (a : l) ++ r = a : (l ++ r)
  '[] ++ r = r

infixr 5 ++
