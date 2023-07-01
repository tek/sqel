module Sqel.Test.ErrorTest where

import qualified Control.Exception as Base
import Control.Exception (evaluate)
import qualified Data.Text as Text
import Hedgehog (TestT, (===))
import Test (unitTest)
import Test.Tasty (TestTree, testGroup)

import Sqel.Test.Error.FragmentMismatch (notAFragment, notRoot, tableForQuery)

typeError ::
  Show a =>
  [Text] ->
  a ->
  TestT IO ()
typeError msg t =
  withFrozenCallStack do
    e <- liftIO $ Base.try @SomeException do
      !a <- show @Text <$> evaluate t
      pure a
    case e of
      Right _ -> fail "Test did not produce an error."
      Left err -> msg === trunc (drop 1 (lines (show err)))
  where
    trunc =
      if null msg
      then id
      else fmap Text.strip . take (length msg)

tableForQueryMessage :: [Text]
tableForQueryMessage =
    [
      "\8226 Cannot use a table fragment for a \8216where\8217 clause."
    ]

notAFragmentMessage :: [Text]
notAFragmentMessage =
    [
      "\8226 The argument of this clause has an invalid type.",
      "It should be one of the fields of the fragments bound by \8216frags <- query\8217,",
      "like \8216frags.tables.users\8217 or \8216frags.query\8217, or combinations of fields like",
      "\8216(frags.users.name, frags.users.address)\8217."

    ]

notRootMessage :: [Text]
notRootMessage =
  [
    "\8226 A \8216from\8217 clause does not accept projections."
  ]

undeterminedParamMessage :: [Text]
undeterminedParamMessage =
  [
    "TODO"
  ]

test_errors :: TestTree
test_errors =
  testGroup "type errors" [
    unitTest "table fragment for query clause" (typeError tableForQueryMessage tableForQuery),
    unitTest "not a fragment" (typeError notAFragmentMessage notAFragment),
    unitTest "not a root fragment" (typeError notRootMessage notRoot)
    -- ,
    -- unitTest "undetermined Dd param" (typeError undeterminedParamMessage undeterminedParam)
  ]
