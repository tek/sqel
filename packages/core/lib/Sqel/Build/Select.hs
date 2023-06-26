module Sqel.Build.Select where

import Sqel.Build.PrimPath (primPathSelector, primPaths)
import Sqel.Data.Field (Field (Field))
import Sqel.Data.SelectorP (SelectorP, formatSelectorP)
import Sqel.Data.Sql (Sql)
import Sqel.Default (Def, SpineDef)
import Sqel.Sql (joinComma)

selectors ::
  Bool ->
  SpineDef ->
  [SelectorP]
selectors multi frag =
  primPathSelector <$> primPaths multi frag

selectorsClause :: Bool -> [Field Def] -> Sql
selectorsClause multi fields =
  joinComma (field =<< fields)
  where
    field (Field s) = formatSelectorP <$> selectors multi s
