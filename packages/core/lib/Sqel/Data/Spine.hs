module Sqel.Data.Spine where

import Data.Aeson (FromJSON, ToJSON)

import Sqel.Data.PgType (PgTypeRef)
import Sqel.Data.PgTypeName (PgCompName)

type PrimFor :: Type -> Type
type family PrimFor tag

type CompFor :: Type -> Type
type family CompFor tag

type CompSort :: Type -> Type
data CompSort tag =
  CompProd
  |
  CompSum (PrimFor tag)
  |
  CompCon
  deriving stock (Generic)

deriving stock instance (Eq (PrimFor tag)) => Eq (CompSort tag)
deriving stock instance (Show (PrimFor tag)) => Show (CompSort tag)

deriving anyclass instance (
    ToJSON (PrimFor tag)
  ) => ToJSON (CompSort tag)

deriving anyclass instance (
    FromJSON (PrimFor tag)
  ) => FromJSON (CompSort tag)

type Spine :: Type -> Type
data Spine tag where
  SpinePrim :: { primMeta :: PrimFor tag } -> Spine tag
  SpineNest :: { compMeta :: CompFor tag, compSort :: CompSort tag, sub :: [Spine tag] } -> Spine tag
  SpineMerge :: { compMeta :: CompFor tag, compSort :: CompSort tag, sub :: [Spine tag] } -> Spine tag
  deriving stock (Generic)

deriving stock instance (Eq (CompFor tag), Eq (PrimFor tag)) => Eq (Spine tag)
deriving stock instance (Show (CompFor tag), Show (PrimFor tag)) => Show (Spine tag)

deriving anyclass instance (
    ToJSON (PrimFor tag),
    ToJSON (CompFor tag)
  ) => ToJSON (Spine tag)

deriving anyclass instance (
    FromJSON (PrimFor tag),
    FromJSON (CompFor tag)
  ) => FromJSON (Spine tag)

spineCompFields :: Spine tag -> Maybe (CompFor tag, CompSort tag, [Spine tag])
spineCompFields = \case
  SpineNest {..} -> Just (compMeta, compSort, sub)
  SpineMerge {..} -> Just (compMeta, compSort, sub)
  SpinePrim _ -> Nothing

pattern SpineComp :: CompFor tag -> CompSort tag -> [Spine tag] -> Spine tag
pattern SpineComp {meta, sort, sub} <- (spineCompFields -> Just (meta, sort, sub))

{-# complete SpineComp, SpinePrim #-}

spineComp :: Bool -> CompFor tag -> CompSort tag -> [Spine tag] -> Spine tag
spineComp = \case
  True -> SpineMerge
  False -> SpineNest

type TypeSpine :: Type -> Type
data TypeSpine tag =
  TypeSpine {
    name :: PgCompName,
    meta :: CompFor tag,
    sub :: [Spine tag]
  }
  deriving stock (Generic)

deriving stock instance (Eq (CompFor tag), Eq (PrimFor tag)) => Eq (TypeSpine tag)
deriving stock instance (Show (CompFor tag), Show (PrimFor tag)) => Show (TypeSpine tag)

deriving anyclass instance (
    ToJSON (PrimFor tag),
    ToJSON (CompFor tag)
  ) => ToJSON (TypeSpine tag)

deriving anyclass instance (
    FromJSON (PrimFor tag),
    FromJSON (CompFor tag)
  ) => FromJSON (TypeSpine tag)

data IndexPrefix =
  IndexPrefix Text
  |
  IndexDefaultPrefix
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Types tag =
  Types {
    table :: Spine tag,
    comp :: Map PgTypeRef (TypeSpine tag)
  }
  deriving stock (Generic)

deriving stock instance (Eq (CompFor tag), Eq (PrimFor tag)) => Eq (Types tag)
deriving stock instance (Show (CompFor tag), Show (PrimFor tag)) => Show (Types tag)

data SpineSort =
  SpineQuery
  |
  SpineTable
  |
  SpineProj

type SpineDesc :: SpineSort -> Symbol
type family SpineDesc sort where
  SpineDesc 'SpineQuery = "query"
  SpineDesc 'SpineTable = "table"
  SpineDesc 'SpineProj = "projection"
