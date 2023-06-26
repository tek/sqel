{-# options_ghc -Wno-redundant-constraints #-}

-- | Description: Type functions
module Sqel.Kind.List where

import Type.Errors (DelayError)

-- | Append two type lists or @Symbol@s.
type (++) :: ∀ k . k -> k -> k
type family (++) l r where
  (++) @[_] (a : l) r = a : (l ++ r)
  (++) @[_] '[] r = r
  (++) @Symbol l r = AppendSymbol l r

infixr 5 ++

showKind ::
  ∀ {k} (a :: k) .
  DelayError ('ShowType a) =>
  ()
showKind =
  ()
