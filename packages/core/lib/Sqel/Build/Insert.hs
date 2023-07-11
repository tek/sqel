module Sqel.Build.Insert where

import Exon (exon)

import Sqel.Build.Columns (rootColumns)
import Sqel.Build.Table (tableName)
import Sqel.Data.Field (RootField)
import Sqel.Data.Sql (Sql)
import Sqel.Data.Def (Def)
import Sqel.Sql (joinComma)

insertIntoClause :: RootField Def -> Sql
insertIntoClause table =
  [exon|#{tableName table} (#{cols})|]
  where
    cols = joinComma (rootColumns table)
