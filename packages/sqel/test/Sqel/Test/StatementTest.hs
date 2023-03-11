module Sqel.Test.StatementTest where

import Test (unitTest)
import Test.Tasty (TestTree, testGroup)

import Sqel.Test.Statement.Con1Test (test_statement_con1)
import Sqel.Test.Statement.HigherOrderTest (
  test_higherOrder2,
  test_statement_higherOrder,
  test_statement_merge_query_higherOrder,
  )
import Sqel.Test.Statement.MergeProdTest (test_statement_merge_prod)
import Sqel.Test.Statement.MergeSumTest (test_statement_merge_sum)
import Sqel.Test.Statement.NamedCompQuery (test_namedCompQuery)
import Sqel.Test.Statement.NewtypeArrayTest (test_statement_newtypeArray)
import Sqel.Test.Statement.NullableQueryTest (test_statement_nullableQuery)
import Sqel.Test.Statement.OrderTest (test_statement_order)
import Sqel.Test.Statement.UpsertTest (test_statement_upsert, test_statement_upsert_sumKey)

test_statement :: TestTree
test_statement =
  testGroup "statement" [
    unitTest "order" test_statement_order,
    unitTest "merge sum" test_statement_merge_sum,
    unitTest "merge prod" test_statement_merge_prod,
    unitTest "higher-order merge statement" test_statement_higherOrder,
    unitTest "higher-order double merge query" test_statement_merge_query_higherOrder,
    unitTest "higher-order with new product class" test_higherOrder2,
    unitTest "unary con with record and positional fields" test_statement_con1,
    unitTest "newtype array" test_statement_newtypeArray,
    unitTest "upsert" test_statement_upsert,
    unitTest "upsert with sum key" test_statement_upsert_sumKey,
    unitTest "composite query with named root" test_namedCompQuery,
    unitTest "query with nullable field" test_statement_nullableQuery
    -- ,
    -- unitTest "join" test_statement_updateFrom
  ]
