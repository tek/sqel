module Sqel.Test.StatementTest where

import Test (unitTest)
import Test.Tasty (TestTree, testGroup)

import Sqel.Test.Statement.BuildTest (test_build)
import Sqel.Test.Statement.Con1Test (test_statement_con1)
import Sqel.Test.Statement.CondExprTest (test_condExpr)
import Sqel.Test.Statement.CustomTagTest (test_customTag)
import Sqel.Test.Statement.DeleteTest (test_delete)
import Sqel.Test.Statement.DropTableTest (test_dropTable)
import Sqel.Test.Statement.EmptyQueryTest (test_statement_emptyQuery)
import Sqel.Test.Statement.IgnoreTest (test_statement_ignore)
import Sqel.Test.Statement.MergeProdTest (test_statement_merge_prod)
import Sqel.Test.Statement.MergeSumTest (test_statement_merge_sum)
import Sqel.Test.Statement.MinimalTest (test_minimal)
import Sqel.Test.Statement.NamedCompQueryTest (test_namedCompQuery)
import Sqel.Test.Statement.NullableQueryTest (test_statement_nullableQuery, test_statement_orNullQuery)
import Sqel.Test.Statement.OrderTest (test_statement_order)
import Sqel.Test.Statement.ParamsTest (test_statement_params)
import Sqel.Test.Statement.ProjectTest (test_project)
import Sqel.Test.Statement.SumQueryTest (test_sumQuery)
import Sqel.Test.Statement.UpsertTest (test_upsert)

test_statement :: TestTree
test_statement =
  testGroup "statement" [
    unitTest "minimal table" test_minimal,
    unitTest "build basics" test_build,
    unitTest "abstract projection" test_project,
    unitTest "order" test_statement_order,
    unitTest "merge sum" test_statement_merge_sum,
    unitTest "merge prod" test_statement_merge_prod,
    unitTest "unary con with record and positional fields" test_statement_con1,
    unitTest "composite query with named root" test_namedCompQuery,
    unitTest "query with nullable field" test_statement_nullableQuery,
    unitTest "query with unguarded nullable field" test_statement_orNullQuery,
    test_delete,
    test_upsert,
    test_dropTable,
    unitTest "custom tag" test_customTag,
    unitTest "sum query" test_sumQuery,
    unitTest "non-condition params" test_statement_params,
    unitTest "ignored query field" test_statement_ignore,
    unitTest "empty query" test_statement_emptyQuery,
    unitTest "custom condition expression" test_condExpr
  ]
