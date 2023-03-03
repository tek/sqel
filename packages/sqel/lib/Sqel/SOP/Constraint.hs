module Sqel.SOP.Constraint where

import Data.Generics.Labels ()
import Generics.SOP (All, All2, Top)
import Generics.SOP.Constraint (Head)
import Generics.SOP.GGP (GCode, GDatatypeInfoOf, GFrom, GTo)
import Generics.SOP.Type.Metadata (DatatypeInfo (ADT, Newtype))

type Coded (d :: Type) (dss :: [[Type]]) =
  GCode d ~ dss

type ProductGCode d =
  Head (GCode d)

type ProductCoded (d :: Type) (ds :: [Type]) =
  Coded d '[ds]

type ReifySOP (d :: Type) (dss :: [[Type]]) =
  (Generic d, GTo d, GCode d ~ dss, All2 Top dss)

type ConstructSOP (d :: Type) (dss :: [[Type]]) =
  (Generic d, GFrom d, GCode d ~ dss, All2 Top dss)

type ReifyProd d ds =
  ReifySOP d '[ds]

type ConstructProd d ds =
  ConstructSOP d '[ds]

type IsNullary =
  (~) '[]

type IsEnum a =
  All IsNullary (GCode a)

type family DatatypeInfoName (dt :: DatatypeInfo) :: Symbol where
  DatatypeInfoName ('ADT _ name _ _) = name
  DatatypeInfoName ('Newtype _ name _) = name

type family DataName (d :: Type) :: Symbol where
  DataName d = DatatypeInfoName (GDatatypeInfoOf d)

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
