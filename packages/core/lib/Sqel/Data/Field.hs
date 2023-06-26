module Sqel.Data.Field where

import Sqel.Data.Spine (PrimFor, Spine)

data Field tag = Field (Spine tag)

deriving stock instance Show (Spine tag) => Show (Field tag)

data CondField tag =
  CondField (Spine tag)
  |
  CondOp Text (Spine tag) (Spine tag)

deriving stock instance Show (Spine tag) => Show (CondField tag)

data PrimField tag = PrimField (PrimFor tag)

deriving stock instance Show (PrimFor tag) => Show (PrimField tag)

data RootField tag = RootField (Spine tag)

deriving stock instance Show (Spine tag) => Show (RootField tag)

data TypeField tag = TypeField (Spine tag)

deriving stock instance Show (Spine tag) => Show (TypeField tag)

data TableField tag = TableField (Spine tag)

deriving stock instance Show (Spine tag) => Show (TableField tag)
