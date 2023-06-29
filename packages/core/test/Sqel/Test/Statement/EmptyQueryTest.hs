module Sqel.Test.Statement.EmptyQueryTest where

import Hedgehog (TestT, (===))

import Sqel.Build.Sql (BuildClause)
import Sqel.Class.Check (Checked1)
import Sqel.Class.ReifySqel (ReifySqel)
import Sqel.Clauses (from, select, where_)
import Sqel.Data.Sqel (SqelFor)
import Sqel.Data.Sql (Sql, sql)
import qualified Sqel.Data.Statement
import Sqel.Data.Statement (Statement)
import Sqel.Dd (DdType, EmptyQuery)
import Sqel.Default (From, Select, Where)
import Sqel.Sqel (emptyQuery)
import Sqel.Syntax.Fragments (query1)
import qualified Sqel.Syntax.Monad as S
import Sqel.Test.Statement.Common (table_Cat)

statement ::
  ReifySqel tag EmptyQuery =>
  Checked1 table tag EmptyQuery =>
  BuildClause tag Select =>
  BuildClause tag From =>
  BuildClause tag Where =>
  SqelFor tag table ->
  Statement () (DdType table)
statement table = S.do
  frags <- query1 emptyQuery table
  select frags.table
  from frags.table
  where_ frags.query

target :: Sql
target = [sql|select "num", "nam", ("fur")."color", ("fur")."density" from "cat"|]

test_statement_emptyQuery :: TestT IO ()
test_statement_emptyQuery = target === (statement table_Cat).sql
