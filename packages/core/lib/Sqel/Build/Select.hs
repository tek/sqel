module Sqel.Build.Select where

import Sqel.Build.PrimPath (renderPrimPaths)
import Sqel.Data.Field (Field (Field))
import Sqel.Data.Sql (Sql)
import Sqel.Default (Def)
import Sqel.Sql (joinComma)

selectorsClause :: Bool -> [Field Def] -> Sql
selectorsClause multi fields =
  joinComma (field =<< fields)
  where
    field (Field s) = renderPrimPaths multi s
