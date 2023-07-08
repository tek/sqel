module Sqel.Data.Statement where

import Exon (exon)
import Hasql.Decoders (Row)
import Hasql.Encoders (Params)

import Sqel.Data.Sql (Sql)
import Sqel.Data.Uid (Uid)

type role Statement nominal nominal nominal
type Statement :: [Type] -> Type -> Type -> Type
data Statement tables query proj =
  Statement {
    sql :: Sql,
    encoder :: Params query,
    decoder :: Row proj
  }

instance Show (Statement tables query result) where
  showsPrec d s = showParen (d > 10) [exon|Statement #{showsPrec 11 s.sql}|]

type UidStatement i table query proj =
  Statement '[Uid i table] query proj

statementSql ::
  ∀ tables query proj .
  Statement tables query proj ->
  Sql
statementSql Statement {sql} = sql
