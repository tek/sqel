module Main where

import Sqel.Test.AbstractTest (test_abstract, test_abstract_query)
import Sqel.Test.DslTest (test_dsl)
import Sqel.Test.EnumTest (test_enum)
import Sqel.Test.ErrorTest (test_errors)
import Sqel.Test.HasGeneric (test_hasGeneric)
import Sqel.Test.JsonTest (test_json)
import Sqel.Test.MigrationTest (test_migration)
import Sqel.Test.PrimTableTest (test_primTable)
import Sqel.Test.SqelTypesTest (test_sqelTypes)
import Sqel.Test.SqlCodeTest (test_sqlCodeNoInterpolation)
import Sqel.Test.Statement.CondOpTest (test_condOp)
import Sqel.Test.StatementTest (test_statement)
import Sqel.Test.SumIdTest (test_sumId)
import Sqel.Test.SyntaxTest (test_syntax)
import Test (unitTest)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "all" [
    unitTest "do syntax" test_syntax,
    test_migration,
    test_errors,
    test_statement,
    test_dsl,
    unitTest "sql quote without interpolation" test_sqlCodeNoInterpolation,
    unitTest "HasGeneric" test_hasGeneric,
    unitTest "Sqel types" test_sqelTypes,
    unitTest "json column" test_json,
    unitTest "enum column" test_enum,
    unitTest "prim table" test_primTable,
    unitTest "abstract column" test_abstract,
    unitTest "abstract column with query" test_abstract_query,
    unitTest "condition operator as mod" test_condOp,
    unitTest "sum type for ID" test_sumId
  ]

main :: IO ()
main =
  defaultMain tests
