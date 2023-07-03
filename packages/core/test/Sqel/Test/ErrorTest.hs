module Sqel.Test.ErrorTest where

import qualified Control.Exception as Base
import Control.Exception (evaluate)
import qualified Data.Text as Text
import Hedgehog (TestT, (===))
import Test (unitTest)
import Test.Tasty (TestTree, testGroup)

import Sqel.Test.Error.FragmentMismatch (notAFragment, notRoot, tableForQuery)
import Sqel.Test.Error.UndeterminedParam (invalidSpec, undeterminedParam)

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
      "• Cannot use a table fragment for a ‘where’ clause."
    ]

notAFragmentMessage :: [Text]
notAFragmentMessage =
    [
      "• The argument of this clause has an invalid type.",
      "It should be one of the fields of the fragments bound by ‘frags <- query’,",
      "like ‘frags.tables.users’ or ‘frags.query’, or combinations of fields like",
      "‘(frags.users.name, frags.users.address)’."

    ]

notRootMessage :: [Text]
notRootMessage =
  [
    "• A ‘from’ clause does not accept projections."
  ]

undeterminedParamMessage :: [Text]
undeterminedParamMessage =
  [
    "• The type (variable) ‘sa’ specifying a column of type ‘Int64’ is undetermined.",
    "If you are calling a polymorphic function that has a constraint like ‘ReifySqel’,",
    "you probably need to use a type application to specify the spec, like ‘Prim’.",
    "If the variable is supposed to be polymorphic, you need to add ‘ReifySqel’ to its function's context",
    "and use ‘sa’ in the type application."
  ]

invalidSpecMessage :: [Text]
invalidSpecMessage =
  [
      "• The spec ‘Int’",
      "given for a column of type ‘Int64’ is not supported.",
      "If you intend to use it as a custom spec, you need to define:",
      "type instance Reify a (Int) = <impl>"
  ]

modOrderMessage :: [Text]
modOrderMessage =
  [
    "• While handling a column mod for a encoder:",
    "‘Sqel.Data.Mods.Nullable.Nullable’",
    "It is declared as creating a ‘Params’, but there is a ‘Value’ mod applied after it.",
    "Try changing the order of mods for this column.",
    "Note that ‘Prim’ might automatically add mods like ‘Nullable’",
    "before your explicit mod, in which case you'll have to make all mods explicit."
  ]

countMismatchMessage :: [Text]
countMismatchMessage =
  [
    "• The number of specified columns does not match the data type.",
    "The type Foo has three fields, but the sqel type specifies two."
  ]

test_errors :: TestTree
test_errors =
  testGroup "type errors" [
    unitTest "table fragment for query clause" (typeError tableForQueryMessage tableForQuery),
    unitTest "not a fragment" (typeError notAFragmentMessage notAFragment),
    unitTest "not a root fragment" (typeError notRootMessage notRoot),
    unitTest "undetermined Dd param" (typeError undeterminedParamMessage undeterminedParam),
    unitTest "invalid spec" (typeError invalidSpecMessage invalidSpec)
    -- TODO
    -- unitTest "count mismatch" (typeError countMismatchMessage countMismatch)
  ]
