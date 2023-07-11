module Sqel.SOP.Constraint where

import Generics.SOP (All, SListI2)
import Generics.SOP.GGP (GCode, GDatatypeInfoOf, GFrom, GTo)
import Generics.SOP.Type.Metadata (DatatypeInfo (ADT, Newtype))
import Type.Reflection (typeRep)

type ReifySOP (d :: Type) (dss :: [[Type]]) =
  (Generic d, GTo d, GCode d ~ dss, SListI2 dss)

type ConstructSOP (d :: Type) (dss :: [[Type]]) =
  (Generic d, GFrom d, GCode d ~ dss, SListI2 dss)

type ReifyProd d ds =
  ReifySOP d '[ds]

type ConstructProd d ds =
  ConstructSOP d '[ds]

type IsNullary =
  (~) '[]

type IsEnum a =
  All IsNullary (GCode a)

type DatatypeInfoName :: DatatypeInfo -> Symbol
type family DatatypeInfoName dt where
  DatatypeInfoName ('ADT _ name _ _) = name
  DatatypeInfoName ('Newtype _ name _) = name

type DataName :: Type -> Symbol
type family DataName d where
  DataName d = DatatypeInfoName (GDatatypeInfoOf d)

type TryDataName' :: Type -> DatatypeInfo -> Constraint
class TryDataName' d info where
  tryDataName' :: Text

instance (
    KnownSymbol name
  ) => TryDataName' d ('ADT m name c s) where
    tryDataName' = symbolText @name

instance (
    KnownSymbol name
  ) => TryDataName' d ('Newtype m name c) where
    tryDataName' = symbolText @name

instance {-# incoherent #-} (
    Typeable d
  ) => TryDataName' d info where
    tryDataName' = show (typeRep @d)

class TryDataName d where
  tryDataName :: Text

instance (
    info ~ GDatatypeInfoOf d,
    TryDataName' d info
  ) => TryDataName d where
  tryDataName = tryDataName' @d @info

symbolString ::
  ∀ (name :: Symbol) .
  KnownSymbol name =>
  String
symbolString =
  symbolVal (Proxy @name)

symbolText ::
  ∀ (name :: Symbol) .
  KnownSymbol name =>
  Text
symbolText =
  toText (symbolString @name)

type KnownSymbolsL :: [Symbol] -> Constraint
class KnownSymbolsL syms where
  symbolStringsL :: [String]

instance KnownSymbolsL '[] where
  symbolStringsL = []

instance (
    KnownSymbol sym,
    KnownSymbolsL syms
  ) => KnownSymbolsL (sym : syms) where
    symbolStringsL = symbolString @sym : symbolStringsL @syms

symbolTextsL ::
  ∀ syms .
  KnownSymbolsL syms =>
  [Text]
symbolTextsL =
  toText <$!> symbolStringsL @syms

type KnownSymbols :: [Symbol] -> Constraint
class KnownSymbols syms where
  symbolStrings :: NonEmpty String

instance (
    KnownSymbol sym,
    KnownSymbolsL syms
  ) => KnownSymbols (sym : syms) where
    symbolStrings = symbolString @sym :| symbolStringsL @syms

symbolTexts ::
  ∀ syms .
  KnownSymbols syms =>
  NonEmpty Text
symbolTexts =
  toText <$!> symbolStrings @syms

natInt ::
  ∀ n .
  KnownNat n =>
  Int
natInt =
  fromInteger (natVal (Proxy @n))
