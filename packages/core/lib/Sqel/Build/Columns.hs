module Sqel.Build.Columns where

import Sqel.Build.Index (prependIndexName)
import Sqel.Data.Field (Field (Field), RootField (RootField))
import Sqel.Data.PgType (columnNameQuoted)
import Sqel.Data.Spine (Spine (SpineMerge, SpineNest, SpinePrim), pattern SpineComp)
import Sqel.Data.Sql (Sql)
import qualified Sqel.Default
import Sqel.Default (CompMeta (CompMeta), Def, PrimMeta (PrimMeta))

spineColumns ::
  (Spine Def -> Bool) ->
  Spine Def ->
  [Sql]
spineColumns cond = \case
  s@(SpinePrim _ PrimMeta {name})
    | cond s -> [columnNameQuoted name]
    | otherwise -> []
  SpineComp _ compSort cols -> prependIndexName compSort (sub =<< cols)
  where
    sub s'
      | cond s' = case s' of
        SpinePrim _ PrimMeta {name} -> [columnNameQuoted name]
        SpineNest CompMeta {name} _ _ -> [columnNameQuoted name]
        SpineMerge _ compSort cols -> prependIndexName compSort (sub =<< cols)
      | otherwise = []

-- TODO isQuery?
fieldColumnsWhere ::
  (Spine Def -> Bool) ->
  Field Def ->
  [Sql]
fieldColumnsWhere cond (Field _ s) = spineColumns cond s

fieldColumns :: Field Def -> [Sql]
fieldColumns = fieldColumnsWhere (const True)

rootColumnsWhere ::
  (Spine Def -> Bool) ->
  RootField Def ->
  [Sql]
rootColumnsWhere cond (RootField (Field _ s)) = spineColumns cond s

rootColumns :: RootField Def -> [Sql]
rootColumns = rootColumnsWhere (const True)
