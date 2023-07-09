module Sqel.Test.MigrationTest where

import Test (unitTest)
import Test.Tasty (TestTree, testGroup)

import Sqel.Test.Migration.ColumnMapTest (test_columnMap)
import Sqel.Test.Migration.ConsistencyTest (test_migrationConsistency)
import Sqel.Test.Migration.RunTest (test_migrationRun)
import Sqel.Test.Migration.SumTest (test_migrationSum)
import Sqel.Test.Migration.SyntaxTest (test_migrationSyntax)

test_migration :: TestTree
test_migration =
  testGroup "migration" [
    unitTest "syntax" test_migrationSyntax,
    unitTest "sum" test_migrationSum,
    test_migrationRun,
    unitTest "consistency" test_migrationConsistency,
    unitTest "column map" test_columnMap
  ]
