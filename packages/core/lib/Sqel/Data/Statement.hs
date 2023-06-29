module Sqel.Data.Statement where

import Exon (exon)
import Hasql.Decoders (Row)
import Hasql.Encoders (Params)

import Sqel.Data.Sql (Sql)

-- TODO add table parameter for safety
-- probably with nominal type role so it can't be faked
data Statement query proj =
  Statement {
    sql :: Sql,
    encoder :: Params query,
    decoder :: Row proj
  }

instance Show (Statement query result) where
  showsPrec d s = showParen (d > 10) [exon|Statement #{showsPrec 11 s.sql}|]

statementSql ::
  ∀ query proj .
  Statement query proj ->
  Sql
statementSql Statement {sql} = sql
