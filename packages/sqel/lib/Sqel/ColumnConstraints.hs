module Sqel.ColumnConstraints where

import Data.Aeson (FromJSON, ToJSON)
import qualified Exon
import Generics.SOP (I (I), NP (Nil, (:*)))
import Lens.Micro ((%~), (.~))

import Sqel.Data.Mods (
  Mods (Mods),
  Nullable (Nullable),
  PgDefault (PgDefault),
  PrimaryKey (PrimaryKey),
  Unique (Unique),
  )
import Sqel.Data.Sql (Sql, ToSql (toSql), sql)

data ConstraintsAcc =
  ConstraintsAcc {
    unique :: Bool,
    nullable :: Bool,
    simple :: [Sql]
  }
  deriving stock (Eq, Show, Generic)

data Constraints =
  Constraints {
    unique :: Bool,
    nullable :: Bool,
    fragments :: [Sql]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance ToSql Constraints where
  toSql Constraints {fragments} = Exon.intercalate " " fragments

class ColumnConstraint mod where
  columnConstraint :: mod -> ConstraintsAcc -> ConstraintsAcc

instance {-# overlappable #-} ColumnConstraint mod where
  columnConstraint _ = id

instance ColumnConstraint Nullable where
  columnConstraint (Nullable _) =
    #nullable .~ True

instance ColumnConstraint PrimaryKey where
  columnConstraint PrimaryKey ConstraintsAcc {..} =
    ConstraintsAcc {
      unique = True,
      simple = "primary key" : simple,
      ..
    }

instance ColumnConstraint PgDefault where
  columnConstraint (PgDefault val) =
    #simple %~ ([sql|default ##{val}|] :)

instance ColumnConstraint Unique where
  columnConstraint Unique ConstraintsAcc {..} =
    ConstraintsAcc {
      unique = True,
      simple = "unique" : simple,
      ..
    }

class ColumnConstraints mods where
  collectConstraints :: NP I mods -> ConstraintsAcc

instance ColumnConstraints '[] where
  collectConstraints Nil = ConstraintsAcc False False []

instance (
    ColumnConstraint mod,
    ColumnConstraints mods
  ) => ColumnConstraints (mod : mods) where
  collectConstraints (I h :* t) =
    columnConstraint h (collectConstraints t)

columnConstraints ::
  ColumnConstraints mods =>
  Mods mods ->
  Constraints
columnConstraints (Mods mods) =
  Constraints {fragments = notNull <> simple, ..}
  where
    notNull | nullable = []
            | otherwise = ["not null"]
    ConstraintsAcc {..} = collectConstraints mods
