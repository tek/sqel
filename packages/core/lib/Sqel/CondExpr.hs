module Sqel.CondExpr where

import Exon (exon)

import Sqel.Data.CondExpr (CondExpr (..))
import Sqel.Data.Path (PrimPath)
import Sqel.Data.Sql (Sql)
import Sqel.Path (renderPrimPath)
import Sqel.Sql.Prepared (dollar)

renderCondExpr ::
  PrimPath ->
  Int ->
  CondExpr ->
  Sql
renderCondExpr path index =
  render
  where
    render = \case
      CondLit a -> a
      CondField -> pathSql
      CondParam -> [exon|#{dollar index}|]
      CondOp op l r -> [exon|#{render l} #{op} #{render r}|]
    pathSql = renderPrimPath path
