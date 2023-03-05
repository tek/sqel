module Main where

import Sqel.Test.ErrorTest (test_errors)
import Sqel.Test.HasGeneric (test_hasGeneric)
import Sqel.Test.MigrationRunTest (test_migrations)
import Sqel.Test.MigrationTest (test_migrationStatements)
import Sqel.Test.QueryProjectionTest (test_queryProjection)
import Sqel.Test.SqlCodeTest (test_sqlCodeNoInterpolation)
import Sqel.Test.StatementTest (test_statement)
import Test (unitTest)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "all" [
    unitTest "migration statements" test_migrationStatements,
    test_migrations,
    test_errors,
    test_statement,
    unitTest "query with a projection" test_queryProjection,
    unitTest "sql quote without interpolation" test_sqlCodeNoInterpolation,
    unitTest "HasGeneric" test_hasGeneric
  ]

main :: IO ()
main =
  defaultMain tests
