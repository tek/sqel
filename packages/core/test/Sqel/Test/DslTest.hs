module Sqel.Test.DslTest where

import Test (unitTest)
import Test.Tasty (TestTree, testGroup)

import Sqel.Test.Dsl.BasicTest (test_dsl_basic)
import Sqel.Test.Dsl.SumTest (test_dsl_con1)

test_dsl :: TestTree
test_dsl =
  testGroup "dsl" [
    unitTest "basic" test_dsl_basic,
    unitTest "unary constructors" test_dsl_con1
  ]
