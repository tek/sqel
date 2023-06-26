module Sqel.Class.DefaultFields where

import Sqel.Data.Field (
  CondField (CondField, CondOp),
  Field (Field),
  PrimField (PrimField),
  RootField (RootField),
  TypeField (TypeField),
  )
import qualified Sqel.Data.Spine
import Sqel.Data.Spine (
  CompFor,
  CompSort (CompCon, CompProd, CompSum),
  PrimFor,
  Spine (SpineMerge, SpineNest, SpinePrim),
  TypeSpine (TypeSpine),
  Types (Types),
  )
import Sqel.Default (CompMeta, Def, PrimMeta)

type DefaultMeta :: Type -> Constraint
class DefaultMeta tag where
  defaultPrimMeta :: PrimFor tag -> PrimMeta
  defaultCompMeta :: CompFor tag -> CompMeta

instance DefaultMeta Def where
  defaultPrimMeta = id
  defaultCompMeta = id

instance {-# overlappable #-} (
    PrimFor tag ~ PrimMeta,
    CompFor tag ~ CompMeta
  ) => DefaultMeta tag where
    defaultPrimMeta = id
    defaultCompMeta = id

defaultSort ::
  ∀ tag .
  DefaultMeta tag =>
  CompSort tag ->
  CompSort Def
defaultSort = \case
  CompProd -> CompProd
  CompSum index -> CompSum (defaultPrimMeta @tag index)
  CompCon -> CompCon

defaultSpine ::
  ∀ tag .
  DefaultMeta tag =>
  Spine tag ->
  Spine Def
defaultSpine = \case
  SpinePrim meta -> SpinePrim (defaultPrimMeta @tag meta)
  SpineNest meta compSort sub -> SpineNest (defaultCompMeta @tag meta) (defaultSort compSort) (defaultSpine <$> sub)
  SpineMerge meta compSort sub -> SpineMerge (defaultCompMeta @tag meta) (defaultSort compSort) (defaultSpine <$> sub)

defaultTypeSpine ::
  ∀ tag .
  DefaultMeta tag =>
  TypeSpine tag ->
  TypeSpine Def
defaultTypeSpine TypeSpine {..} =
  TypeSpine {meta = defaultCompMeta @tag meta, sub = defaultSpine <$> sub, ..}

defaultTypes ::
  ∀ tag .
  DefaultMeta tag =>
  Types tag ->
  Types Def
defaultTypes Types {..} =
  Types {table = defaultSpine table, comp = defaultTypeSpine <$> comp}

type DefaultFields :: Type -> Type -> Constraint
class DefaultFields fields fieldsDef where
  defaultFields :: fields -> fieldsDef

instance (
    DefaultFields field fieldDef
  ) => DefaultFields [field] [fieldDef] where
    defaultFields = fmap defaultFields

instance (
    DefaultMeta tag
  ) => DefaultFields (Field tag) (Field Def) where
    defaultFields (Field s) = Field (defaultSpine s)

instance (
    DefaultMeta tag
  ) => DefaultFields (CondField tag) (CondField Def) where
    defaultFields = \case
      CondField s -> CondField (defaultSpine s)
      CondOp op l r -> CondOp op (defaultSpine l) (defaultSpine r)

instance (
    DefaultMeta tag
  ) => DefaultFields (PrimField tag) (PrimField Def) where
    defaultFields (PrimField s) = PrimField (defaultPrimMeta @tag s)

instance (
    DefaultMeta tag
  ) => DefaultFields (RootField tag) (RootField Def) where
    defaultFields (RootField s) = RootField (defaultSpine s)

instance (
    DefaultMeta tag
  ) => DefaultFields (TypeField tag) (TypeField Def) where
    defaultFields (TypeField s) = TypeField (defaultSpine s)
