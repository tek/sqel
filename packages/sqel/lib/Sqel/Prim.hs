module Sqel.Prim where

import Data.Aeson (FromJSON, ToJSON)
import Generics.SOP (I (I), NP (Nil, (:*)))

import Sqel.Class.Mods (SymNP, symMods)
import Sqel.Column (nullable)
import Sqel.Data.Dd (ConCol, Dd (Dd), DdK (DdK), DdStruct (DdPrim), Sqel', Struct (Prim))
import Sqel.Data.Mods (
  ArrayColumn (ArrayColumn),
  EnumColumn,
  Ignore (Ignore),
  Mods (Mods),
  Newtype (Newtype),
  pattern NoMods,
  type NoMods,
  Nullable,
  ReadShowColumn,
  )
import Sqel.Data.PgType (PgPrimName)
import Sqel.Data.Sel (
  IndexName,
  Sel (SelAuto, SelIndex, SelSymbol, SelUnused),
  SelPrefix (DefaultPrefix),
  SelW (SelWAuto, SelWIndex),
  )
import Sqel.Mods (PrimValueCodec, primEnumMods, primJsonMods, primReadShowMods)
import Sqel.Names (named, selAs)
import Sqel.SOP.Constraint (ProductGCode)
import Sqel.SOP.Error (Quoted)
import Sqel.SOP.Newtype (UnwrapNewtype (unwrapNewtype, wrapNewtype))

type IndexColumnWith prefix name =
  'DdK ('SelIndex prefix name) NoMods Int64 'Prim

type IndexColumn name =
  IndexColumnWith 'DefaultPrefix name

column :: Mods mods -> Dd ('DdK 'SelAuto mods a 'Prim)
column m =
  Dd SelWAuto m DdPrim

mods ::
  SymNP mods ps =>
  mods ->
  Mods ps
mods =
  symMods

primMod ::
  mods ->
  Dd ('DdK 'SelAuto '[mods] a 'Prim)
primMod ms =
  column (Mods (I ms :* Nil))

primMods ::
  SymNP mods ps =>
  mods ->
  Dd ('DdK 'SelAuto ps a 'Prim)
primMods ms =
  column (mods ms)

prim ::
  ∀ a .
  Dd ('DdK 'SelAuto NoMods a 'Prim)
prim =
  column NoMods

ignore ::
  ∀ a .
  Dd ('DdK 'SelUnused '[Ignore] a 'Prim)
ignore =
  selAs (primMod Ignore)

-- TODO try to use WhenStuck to decide whether to use the type arg in UnwrapNewtype to print the suggested decl with
-- Generic
type NewtypeError combi =
  Quoted combi <> " declares a column for a newtype using " <> Quoted "Generic" <> "."

-- TODO check whether this should be DdK-polymorphic. consumers get very verbose in the structs when it is, but they
-- have more quantifiers when it isn't.
type NewtypeWrap :: Symbol -> Type -> Type -> Constraint
class NewtypeWrap combi a w | a -> w where
  newtypeWrap :: Sqel' sel mods w s -> Sqel' sel (Newtype a w : mods) a s

instance (
    err ~ NewtypeError combi,
    UnwrapNewtype err a w
  ) => NewtypeWrap combi a w where
    newtypeWrap (Dd sel (Mods ms) s) =
      Dd sel (Mods (I (Newtype @a @w (unwrapNewtype @err) (wrapNewtype @err) ):* ms)) s

newtyped ::
  ∀ sel mods a w s .
  NewtypeWrap "newtyped" a w =>
  Sqel' sel mods w s ->
  Sqel' sel (Newtype a w : mods) a s
newtyped =
  newtypeWrap @"newtyped"

primNewtype ::
  ∀ a w .
  NewtypeWrap "primNewtype" a w =>
  Dd ('DdK 'SelAuto '[Newtype a w] a 'Prim)
primNewtype =
  newtypeWrap @"primNewtype" prim

primCoerce ::
  ∀ a w .
  Coercible a w =>
  Dd ('DdK 'SelAuto '[Newtype a w] a 'Prim)
primCoerce =
  primMod (Newtype coerce coerce)

primIndex ::
  ∀ tpe name .
  IndexName 'DefaultPrefix tpe name =>
  Dd (IndexColumn tpe)
primIndex =
  Dd (SelWIndex Proxy) NoMods DdPrim

-- TODO move aeson to reify
json ::
  ∀ a .
  ToJSON a =>
  FromJSON a =>
  Dd ('DdK 'SelAuto [PgPrimName, PrimValueCodec a] a 'Prim)
json =
  column primJsonMods

enum ::
  ∀ a .
  Dd ('DdK 'SelAuto [PgPrimName, EnumColumn] a 'Prim)
enum =
  column primEnumMods

readShow ::
  ∀ a .
  Dd ('DdK 'SelAuto [PgPrimName, ReadShowColumn] a 'Prim)
readShow =
  column primReadShowMods

primNullable ::
  ∀ a .
  Dd ('DdK 'SelAuto '[Nullable] (Maybe a) 'Prim)
primNullable =
  nullable (prim @a)

primAs ::
  ∀ name a .
  KnownSymbol name =>
  Dd ('DdK ('SelSymbol name) '[] a 'Prim)
primAs =
  named @name (prim @a)

-- TODO are composite arrays legal?
array ::
  ∀ f a mods sel .
  Dd ('DdK sel mods a 'Prim) ->
  Dd ('DdK sel (ArrayColumn f : mods) (f a) 'Prim)
array (Dd sel (Mods ms) s) =
  Dd sel (Mods (I ArrayColumn :* ms)) s

newtype Prims a s =
  Prims { unPrims :: NP Dd s }
  deriving stock (Generic)

class MkPrims as s | as -> s where
  mkPrims :: NP Dd s

instance MkPrims '[] '[] where
  mkPrims = Nil

instance (
    MkPrims as s
  ) => MkPrims (a : as) ('DdK 'SelAuto '[] a 'Prim : s) where
    mkPrims = prim :* mkPrims @as @s

type family PrimProd (a :: Type) :: [Type] where
  PrimProd (ConCol _ _ _ as) = as
  PrimProd a = ProductGCode a

prims ::
  ∀ (a :: Type) (s :: [DdK]) .
  MkPrims (PrimProd a) s =>
  Prims a s
prims =
  Prims (mkPrims @(PrimProd a))

class MkPrimNewtypes as s | as -> s where
  mkPrimNewtypes :: NP Dd s

instance MkPrimNewtypes '[] '[] where
  mkPrimNewtypes = Nil

instance (
    MkPrimNewtypes as s,
    NewtypeWrap "primNewtypes" a w
  ) => MkPrimNewtypes (a : as) ('DdK 'SelAuto '[Newtype a w] a 'Prim : s) where
    mkPrimNewtypes = newtypeWrap @"primNewtypes" prim :* mkPrimNewtypes @as @s

primNewtypes ::
  ∀ (a :: Type) (s :: [DdK]) .
  MkPrimNewtypes (PrimProd a) s =>
  Prims a s
primNewtypes =
  Prims (mkPrimNewtypes @(PrimProd a))
