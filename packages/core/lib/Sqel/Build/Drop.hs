module Sqel.Build.Drop where

import Exon (exon)

import Sqel.Build.Table (tableName)
import Sqel.Data.Clause (ClauseParam (ClauseParam))
import qualified Sqel.Data.Drop
import Sqel.Data.Drop (Cascade (Cascade, Restrict), Drop (Drop))
import Sqel.Data.Field (RootField)
import Sqel.Data.Sql (Sql)
import Sqel.Default (Def)

dropTableClause :: ClauseParam (RootField Def) Drop -> Sql
dropTableClause (ClauseParam field Drop {..}) =
  [exon|#{ex}#{tableName field}#{casc}|]
  where
    ex = if ifExists then "if exists " else ""
    casc = case cascade of
      Just Cascade -> " cascade"
      Just Restrict -> " restrict"
      Nothing -> ""
