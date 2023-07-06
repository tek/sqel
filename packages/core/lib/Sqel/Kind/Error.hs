module Sqel.Kind.Error where

type PlainTypeError :: k0 -> k
type family PlainTypeError msg where
  PlainTypeError msg = TypeError (ToErrorMessage msg)
