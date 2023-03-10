module Sqel.Text.DbIdentifier where

import Data.List (dropWhileEnd)

import Sqel.SOP.Constraint (symbolString)
import Sqel.Text.Case (unCamelCase)
import Sqel.Text.Quote (dquote)

dbIdentifier :: String -> Text
dbIdentifier =
  unCamelCase '_' . dropWhile ('_' ==) . dropWhileEnd ('_' ==)

dbIdentifierT :: Text -> Text
dbIdentifierT =
  dbIdentifier . toString

quotedDbId :: Text -> Text
quotedDbId =
  dquote . dbIdentifierT

dbSymbol ::
  ∀ name .
  KnownSymbol name =>
  Text
dbSymbol =
  dbIdentifier (symbolString @name)
