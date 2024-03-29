module Sqel.Test.Statement.CondExprTest where

import Generics.SOP (NP (Nil, (:*)))
import Hedgehog (TestT, (===))

import Sqel.Class.ReifySqel (sqel)
import Sqel.Clauses (from, select, where_)
import Sqel.Data.CondExpr (CondExpr (CondCall, CondField, CondLit, CondOp, CondParam))
import Sqel.Data.Sqel (SqelFor (SqelComp, SqelPrim))
import Sqel.Data.Sql (Sql, sql)
import Sqel.Data.Statement (statementSql)
import Sqel.Data.TestTables (Table_Simp)
import Sqel.Default (Sqel, withCondExpr)
import Sqel.Dsl (Gen, Query)
import Sqel.Syntax.Fragments (query)
import qualified Sqel.Syntax.Monad as S

data Qn =
  Qn {
    name :: Text,
    number :: Int64
  }
  deriving stock (Eq, Show, Generic)

type Query_Qn = Query Qn Gen

sqel_Nc :: Sqel Table_Simp
sqel_Nc = sqel

expr :: CondExpr
expr =
  CondOp ">" CondField (CondCall "exp" [CondOp "*" CondParam (CondLit "5")])

sqel_Qn :: Sqel Query_Qn
sqel_Qn = case sqel @Query_Qn of
  SqelComp i meta s (n :* SqelPrim table pmeta pcodec :* Nil) codec ->
    SqelComp i meta s (n :* SqelPrim table (withCondExpr expr pmeta) pcodec :* Nil) codec

stmt_condExpr :: Sql
stmt_condExpr = statementSql S.do
  fs <- query sqel_Qn sqel_Nc
  select fs.table
  from fs.table
  where_ fs.query

test_condExpr :: TestT IO ()
test_condExpr =
  [sql|select "name", "number" from "simp" where "name" = $1 and "number" > exp($2 * 5)|] === stmt_condExpr
