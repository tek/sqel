module Main where

import Hedgehog (TestT, property, test, withTests)
import Sqel.Test.InitTest (test_init)
import Sqel.Test.JoinTest (test_join)
import Sqel.Test.SumTest (test_sum)
import Test.Tasty (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)

unitTest ::
  TestName ->
  TestT IO () ->
  TestTree
unitTest desc =
  testProperty desc . withTests 1 . property . test

tests :: TestTree
tests =
  testGroup "integration" [
    unitTest "init table" test_init,
    unitTest "join statement" test_join,
    unitTest "sum table" test_sum
  ]

main :: IO ()
main = defaultMain tests
