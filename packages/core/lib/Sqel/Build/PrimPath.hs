module Sqel.Build.PrimPath where

import Sqel.Build.Index (prependIndexWith)
import Sqel.Data.PgTypeName (PgTableName)
import qualified Sqel.Data.SelectorP
import Sqel.Data.SelectorP (SelectorP (SelectorP))
import Sqel.Data.Spine (Spine (SpinePrim), pattern SpineComp, SpinePath (SpinePath))
import qualified Sqel.Default
import Sqel.Default (CompMeta, PrimMeta, SpineDef)

data PrimPath =
  PrimPath {
    table :: Maybe PgTableName,
    path :: SpinePath
  }
  deriving stock (Eq, Show, Generic)

primPathWith ::
  Bool ->
  PgTableName ->
  SpinePath ->
  PrimPath
primPathWith multi mtable path =
  PrimPath table path
  where
    table | multi = Just mtable
          | otherwise = Nothing

primPath ::
  Bool ->
  PrimMeta ->
  PrimPath
primPath multi meta =
  primPathWith multi meta.table meta.path

primPathSelector ::
  PrimPath ->
  SelectorP
primPathSelector (PrimPath table (SpinePath (root :| sub))) =
  SelectorP {..}

indexPath :: Bool -> CompMeta -> PrimMeta -> PrimPath
indexPath multi meta index =
  primPathWith multi meta.table index.path

primPaths ::
  Bool ->
  SpineDef ->
  [PrimPath]
primPaths multi =
  spin
  where
    spin = \case
      SpinePrim meta -> [primPath multi meta]
      SpineComp meta compSort sub -> prependIndexWith (indexPath multi meta) compSort (spin =<< sub)
