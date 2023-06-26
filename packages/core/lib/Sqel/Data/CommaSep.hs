module Sqel.Data.CommaSep where

import qualified Exon

import Sqel.Data.Sql (ToSql (toSql))

newtype CommaSep a =
  CommaSep { unCommaSep :: a }
  deriving stock (Eq, Show, Generic)

instance ToSql a => ToSql (CommaSep [a]) where
  toSql (CommaSep a) =
    Exon.intercalate ", " (toSql <$> a)
