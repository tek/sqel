module Sqel.Data.CondExpr where

import Data.Aeson (FromJSON, ToJSON)

import Sqel.Data.Sql (Sql)

data CondExpr =
  CondLit Sql
  |
  CondField
  |
  CondParam
  |
  CondOp Sql CondExpr CondExpr
  |
  CondCall Sql [CondExpr]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
