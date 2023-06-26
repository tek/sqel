module Sqel.Reify.PrimName where

import Sqel.Codec (PrimColumnOrError (PrimTypeNameOrError))
import Sqel.Data.Mods.Array (Array)
import Sqel.Data.Mods.Json (Json, Jsonb)
import Sqel.Data.Mods.Name (SetPrimName)
import Sqel.Data.Mods.Newtype (Newtype)
import Sqel.Data.Mods.Nullable (Nullable)
import Sqel.Data.PgType (PgPrimName, pgPrimName)
import Sqel.Default (Def)
import Sqel.Error.PrimCodec (ArrayMismatch)
import Sqel.Kind.List (type (++))

-- TODO column name
-- TODO same for Nullable
type PrimNameArray :: (Type -> Type) -> Type -> [Type] -> Symbol
type family PrimNameArray f a mods where
  PrimNameArray f (f a) mods = PrimNameDef a mods ++ "[]"
  PrimNameArray f a _ = ArrayMismatch f a

type PrimNameDef :: Type -> [Type] -> Symbol
type family PrimNameDef a mods where
  PrimNameDef a '[] = PrimTypeNameOrError a
  PrimNameDef (Maybe a) (Nullable _ : mods) = PrimNameDef a mods
  PrimNameDef a (Array f : mods) = PrimNameArray f a mods
  PrimNameDef a (Newtype a w : mods) = PrimNameDef w mods
  PrimNameDef _ (SetPrimName name : _) = name
  PrimNameDef _ (Json : _) = "json"
  PrimNameDef _ (Jsonb : _) = "jsonb"
  PrimNameDef a (_ : mods) = PrimNameDef a mods

type PrimNameCustom :: Type -> Type -> [Type] -> Symbol
type family PrimNameCustom tag a mods

type PrimName :: Type -> Type -> [Type] -> Symbol
type family PrimName tag a mods where
  PrimName Def a mods = PrimNameDef a mods
  PrimName tag a mods = PrimNameCustom tag a mods

reifyPrimName ::
  ∀ tag a mods .
  KnownSymbol (PrimName tag a mods) =>
  PgPrimName
reifyPrimName =
  pgPrimName @(PrimName tag a mods)
