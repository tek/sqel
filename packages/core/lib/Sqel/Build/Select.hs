module Sqel.Build.Select where

import Sqel.Build.PrimPath (renderPrimPaths)
import Sqel.Data.Def (Def)
import Sqel.Data.Field (Field (Field))
import Sqel.Data.Sql (Sql)
import Sqel.Sql (joinComma)

selectorsClause :: Bool -> [Field Def] -> Sql
selectorsClause multi fields =
  joinComma (field =<< fields)
  where
    field (Field _ s) = renderPrimPaths multi s
