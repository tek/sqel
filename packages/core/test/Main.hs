module Main where

import Sqel.Test.DslTest (test_dsl)
import Sqel.Test.EnumTest (test_enum)
import Sqel.Test.ErrorTest (test_errors)
import Sqel.Test.HasGeneric (test_hasGeneric)
import Sqel.Test.JsonTest (test_json)
import Sqel.Test.MigrationTest (test_migration)
import Sqel.Test.SqelTypesTest (test_sqelTypes)
import Sqel.Test.SqlCodeTest (test_sqlCodeNoInterpolation)
import Sqel.Test.StatementTest (test_statement)
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
    unitTest "enum column" test_enum
  ]

main :: IO ()
main =
  defaultMain tests
