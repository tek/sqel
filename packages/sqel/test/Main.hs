module Main where

import Sqel.Test.ErrorTest (test_errors)
import Sqel.Test.HasGeneric (test_hasGeneric)
import Sqel.Test.MigrationRunTest (test_migrationTransformAbsent)
import Sqel.Test.MigrationTest (test_migration)
import Sqel.Test.QueryProjectionTest (test_queryProjection)
import Sqel.Test.SqlCodeTest (test_sqlCodeNoInterpolation)
import Sqel.Test.StatementTest (test_statement)
import Test (unitTest)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "all" [
    unitTest "migration" test_migration,
    unitTest "run migration" test_migrationTransformAbsent,
    test_errors,
    test_statement,
    unitTest "query with a projection" test_queryProjection,
    unitTest "sql quote without interpolation" test_sqlCodeNoInterpolation,
    unitTest "HasGeneric" test_hasGeneric,
    unitTest "migration" test_migration
  ]

main :: IO ()
main =
  defaultMain tests
