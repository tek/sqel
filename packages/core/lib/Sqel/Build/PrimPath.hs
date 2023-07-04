module Sqel.Build.PrimPath where

import Sqel.Build.Index (prependIndexWith)
import Sqel.Data.Path (PrimPath)
import Sqel.Data.Spine (Spine (SpinePrim), pattern SpineComp)
import Sqel.Data.Sql (Sql)
import qualified Sqel.Default
import Sqel.Default (CompMeta, PrimMeta, SpineDef)
import Sqel.Path (primMetaPath, primPath, renderPrimPath)

indexPath :: Bool -> CompMeta -> PrimMeta -> PrimPath
indexPath multi meta index =
  primPath multi meta.table index.path

primPaths ::
  Bool ->
  SpineDef ->
  [PrimPath]
primPaths multi =
  spin
  where
    spin = \case
      SpinePrim meta -> [primMetaPath multi meta]
      SpineComp meta compSort sub -> prependIndexWith (indexPath multi meta) compSort (spin =<< sub)

renderPrimPaths ::
  Bool ->
  SpineDef ->
  [Sql]
renderPrimPaths multi frag =
  renderPrimPath <$> primPaths multi frag
