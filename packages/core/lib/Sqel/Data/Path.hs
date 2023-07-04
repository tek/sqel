module Sqel.Data.Path where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Exts (IsList)

import Sqel.Data.PgTypeName (PgTableName)

newtype FieldPath =
  FieldPath { unSpinePath :: NonEmpty Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON, IsList)

data PrimPath =
  PrimPath {
    table :: Maybe PgTableName,
    path :: FieldPath
  }
  deriving stock (Eq, Show, Generic)
