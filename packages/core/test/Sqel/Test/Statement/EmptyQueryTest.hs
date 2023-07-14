module Sqel.Test.Statement.EmptyQueryTest where

import Hedgehog (TestT, (===))

import Sqel.Build.Sql (BuildClause)
import Sqel.Class.Check (Check1)
import Sqel.Class.ReifySqel (ReifySqelFor)
import Sqel.Clauses (from, select, where_)
import Sqel.Data.Sqel (SqelFor)
import Sqel.Data.Sql (Sql, sql)
import qualified Sqel.Data.Statement
import Sqel.Data.Statement (Statement)
import Sqel.Data.TestTables (table_Cat)
import Sqel.Dd (DdType, EmptyQuery)
import Sqel.Default (From, Select, Where)
import Sqel.Sqel (emptyQuery)
import Sqel.Syntax.Fragments (query)
import qualified Sqel.Syntax.Monad as S

statement ::
  ReifySqelFor tag EmptyQuery =>
  Check1 table EmptyQuery =>
  BuildClause tag Select =>
  BuildClause tag From =>
  BuildClause tag Where =>
  SqelFor tag table ->
  Statement '[DdType table] () (DdType table)
statement table = S.do
  frags <- query emptyQuery table
  select frags.table
  from frags.table
  where_ frags.query

target :: Sql
target = [sql|select "num", "nam", ("fur")."color", ("fur")."density" from "cat"|]

test_statement_emptyQuery :: TestT IO ()
test_statement_emptyQuery = target === (statement table_Cat).sql
