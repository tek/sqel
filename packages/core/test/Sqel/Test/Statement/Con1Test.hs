module Sqel.Test.Statement.Con1Test where

import Hedgehog (TestT, (===))

import Sqel.Clauses (from, select)
import Sqel.Data.Sql (Sql, sql)
import Sqel.Data.Statement (statementSql)
import Sqel.Syntax.Fragments (table)
import qualified Sqel.Syntax.Monad as S
import Sqel.Test.Statement.Common (table_NaNu)

statement_con1 :: Sql
statement_con1 = statementSql S.do
  t <- table table_NaNu
  select t
  from t

test_statement_con1 :: TestT IO ()
test_statement_con1 =
  [sql|select "sqel_sum_index__na_nu", "name", "nu" from "na_nu"|] === statement_con1
