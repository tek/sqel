module Sqel.Build.Index where

import Sqel.Class.DefaultFields (DefaultMeta (defaultPrimMeta))
import Sqel.Data.PgType (columnNameQuoted)
import Sqel.Data.Spine (CompSort (CompSum), PrimFor, Spine (SpinePrim))
import Sqel.Data.Sql (Sql)
import qualified Sqel.Default
import Sqel.Default (PrimMeta (PrimMeta))

renderIndexName ::
  ∀ tag .
  DefaultMeta tag =>
  PrimFor tag ->
  Sql
renderIndexName (defaultPrimMeta @tag -> PrimMeta {name}) = columnNameQuoted name

indexWith :: (PrimFor tag -> Maybe a) -> CompSort tag -> Maybe a
indexWith f = \case
  CompSum index -> f index
  _ -> Nothing

prependIndexWithMaybe :: (PrimFor tag -> Maybe a) -> CompSort tag -> [a] -> [a]
prependIndexWithMaybe f meta as =
  maybeToList (indexWith f meta) ++ as

prependIndexWith :: (PrimFor tag -> a) -> CompSort tag -> [a] -> [a]
prependIndexWith f =
  prependIndexWithMaybe (Just . f)

prependIndexName ::
  ∀ tag .
  DefaultMeta tag =>
  CompSort tag ->
  [Sql] ->
  [Sql]
prependIndexName = prependIndexWith (renderIndexName @tag)

prependIndex ::
  CompSort tag ->
  [Spine tag] ->
  [Spine tag]
prependIndex = prependIndexWith SpinePrim
