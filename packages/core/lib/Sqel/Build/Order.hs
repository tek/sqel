module Sqel.Build.Order where

import Exon (exon)

import Sqel.Build.PrimPath (primPath, primPathSelector)
import Sqel.Data.Clause (ClauseParam (ClauseParam))
import Sqel.Data.Field (PrimField (PrimField))
import Sqel.Data.Order (Order)
import Sqel.Data.SelectorP (formatSelectorP)
import Sqel.Data.Sql (Sql)
import Sqel.Default (Def)

-- TODO order by comp?
orderByClause :: Bool -> (ClauseParam (PrimField Def) Order) -> Sql
orderByClause multi (ClauseParam (PrimField meta) order) =
  [exon|#{sel} ##{order}|]
  where
    sel = formatSelectorP (primPathSelector (primPath multi meta))
