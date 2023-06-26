module Sqel.Data.Drop where

data Cascade =
  Cascade
  |
  Restrict
  deriving stock (Eq, Show, Generic)

data Drop =
  Drop {
    ifExists :: Bool,
    cascade :: Maybe Cascade
  }
  deriving stock (Eq, Show, Generic)
