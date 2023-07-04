module Sqel.Data.Selector where

import Prettyprinter (Pretty (pretty))

import Sqel.Data.Sql (Sql (Sql), ToSql (toSql))

newtype Selector =
  Selector { unSelector :: Sql }
  deriving stock (Eq, Show, Generic, Ord)
  deriving newtype (IsString, Semigroup, Monoid)

instance Pretty Selector where
  pretty (Selector s) = pretty s

instance ToSql Selector where
  toSql = (.unSelector)
