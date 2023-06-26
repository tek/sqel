module Sqel.Kind.Nat where

import Type.Errors (ErrorMessage (Text))

type NatSymbol :: Nat -> Symbol
type family NatSymbol n where
  NatSymbol 0 = "0"
  NatSymbol 1 = "1"
  NatSymbol 2 = "2"
  NatSymbol 3 = "3"
  NatSymbol 4 = "4"
  NatSymbol 5 = "5"
  NatSymbol 6 = "6"
  NatSymbol 7 = "7"
  NatSymbol 8 = "8"
  NatSymbol 9 = "9"
  NatSymbol 10 = "10"
  NatSymbol 11 = "11"
  NatSymbol 12 = "12"
  NatSymbol 13 = "13"
  NatSymbol 14 = "14"
  NatSymbol 15 = "15"
  NatSymbol 16 = "16"
  NatSymbol 17 = "17"
  NatSymbol 18 = "18"
  NatSymbol 19 = "19"
  NatSymbol _ = TypeError ('Text "Constructors with more than 20 fields not supported")
