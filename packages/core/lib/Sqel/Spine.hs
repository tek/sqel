module Sqel.Spine where

import Data.Some (Some (Some))

import Sqel.Build.Index (prependIndex)
import Sqel.Class.DefaultFields (DefaultMeta (defaultCompMeta, defaultPrimMeta))
import Sqel.Data.PgType (PgColumnName, PgTypeRef)
import Sqel.Data.PgTypeName (pattern PgCompName, PgTableName, pattern PgTypeName, pgTableName)
import qualified Sqel.Data.Spine
import Sqel.Data.Spine (
  CompSort (CompSum),
  Spine (SpineMerge, SpineNest, SpinePrim),
  pattern SpineComp,
  TypeSpine (TypeSpine),
  Types (Types),
  )
import qualified Sqel.Default
import Sqel.Default (CompMeta (CompMeta))

spineComps ::
  ∀ tag .
  DefaultMeta tag =>
  Spine tag ->
  Map PgTypeRef (TypeSpine tag)
spineComps = \case
  SpineNest meta@(defaultCompMeta @tag -> CompMeta {colType, typeName = Some name@(PgCompName _)}) _ sub ->
    [(colType, TypeSpine name meta sub)] <> foldMap spineComps sub
  SpineMerge _ _ sub ->
    foldMap spineComps sub
  _ -> mempty

spineTypes ::
  DefaultMeta tag =>
  Spine tag ->
  Types tag
spineTypes table =
  Types table case table of
    SpineComp _ _ sub ->
      foldMap spineComps sub
    SpinePrim _ _ ->
      mempty

spineTableName ::
  ∀ tag .
  DefaultMeta tag =>
  Spine tag ->
  PgTableName
spineTableName = \case
  SpineComp (defaultCompMeta @tag -> CompMeta {typeName = Some (PgTypeName name)}) _ _ -> pgTableName name
  SpinePrim table _ -> table

mergeCols :: [Spine tag] -> [Spine tag]
mergeCols cols =
  cols >>= \case
    s@SpinePrim {} -> [s]
    s@SpineNest {} -> [s]
    SpineMerge {compSort, sub} -> prependIndex compSort (mergeCols sub)

spineTypeCols :: Spine tag -> [Spine tag]
spineTypeCols = \case
  s@(SpinePrim _ _) -> [s]
  SpineComp _ compSort sub -> prependIndex compSort (mergeCols sub)

spineColumnName ::
  ∀ tag .
  DefaultMeta tag =>
  Spine tag ->
  PgColumnName
spineColumnName = \case
  SpinePrim _ (defaultPrimMeta @tag -> meta) -> meta.name
  SpineComp (defaultCompMeta @tag -> meta) _ _ -> meta.name

isSum :: CompSort tag -> Bool
isSum = \case
  CompSum _ _ -> True
  _ -> False
