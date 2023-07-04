module Sqel.Data.QueryMeta where

import Data.Aeson (FromJSON, ToJSON)

import Sqel.Data.CondExpr (CondExpr)
import Sqel.Data.Sql (Sql)

data CondCode =
  CondExpr CondExpr
  |
  CondOp Sql
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data CondMeta =
  CondMeta {
    code :: CondCode,
    nullable :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data QueryMeta =
  QuerySynthetic
  |
  QueryMeta {
    index :: Int,
    cond :: Maybe CondMeta
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
