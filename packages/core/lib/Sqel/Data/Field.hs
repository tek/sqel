module Sqel.Data.Field where

import Sqel.Data.Spine (PrimFor, Spine)
import Sqel.Data.Sql (Sql)

data Field tag =
  Field {
    query :: Bool,
    spine :: Spine tag
  }

deriving stock instance Show (Spine tag) => Show (Field tag)

data CondOperand tag =
  CondOpField (Field tag)
  |
  CondOpLit Sql

deriving stock instance Show (Spine tag) => Show (CondOperand tag)

data CondField tag =
  CondField (Field tag)
  |
  CondOp Text (CondOperand tag) (CondOperand tag)

deriving stock instance Show (Spine tag) => Show (CondField tag)

data PrimField tag =
  PrimField {
    query :: Bool,
    meta :: PrimFor tag
  }

deriving stock instance Show (PrimFor tag) => Show (PrimField tag)

data RootField tag = RootField (Field tag)

deriving stock instance Show (Spine tag) => Show (RootField tag)

data TypeField tag = TypeField (Field tag)

deriving stock instance Show (Spine tag) => Show (TypeField tag)

data TableField tag = TableField (Field tag)

deriving stock instance Show (Spine tag) => Show (TableField tag)

type OrLiteral :: Type -> Type -> Type
data OrLiteral lit field =
  LiteralField lit
  |
  NotLiteral field

deriving stock instance (
    Show lit,
    Show field
  ) => Show (OrLiteral lit field)
