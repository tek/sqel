module Sqel.Migration.Data.TypeStatus where

data TypeStatus =
  Absent
  |
  Mismatch
  |
  Match
  deriving stock (Eq, Show, Generic)
