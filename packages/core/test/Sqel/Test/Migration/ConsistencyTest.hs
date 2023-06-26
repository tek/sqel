module Sqel.Test.Migration.ConsistencyTest where

import qualified Data.Aeson as Aeson
import Hedgehog (TestT, evalEither, (===))
import Prelude hiding (Default)

import Sqel.Class.ReifySqel (sqel)
import Sqel.Data.Migration (Migrate)
import Sqel.Default (Def, Sqel)
import Sqel.Dsl
import Sqel.Migration.Class.Syntax ((-->))
import Sqel.Migration.Consistency (migrationsMetadata)
import Sqel.Migration.Consistency.Check (checkMigrationConsistency)
import Sqel.Test.Migration.ConsistencyData (meta1)

data PordOld =
  PordOld {
    p1 :: Int64
  }
  deriving stock (Eq, Show, Generic)

data Pord =
  Pord {
    p1 :: Int64,
    p2 :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

data Dat0 =
  Dat0 {
    old :: Text
  }
  deriving stock (Eq, Show, Generic)

data Dat1 =
  Dat1 {
    size :: Int64,
    pord :: PordOld
  }
  deriving stock (Eq, Show, Generic)

data Dat2 =
  Dat2 {
    number :: Int64,
    pord :: Pord
  }
  deriving stock (Eq, Show, Generic)

data Dat =
  Dat {
    name :: Text,
    num :: Int64,
    pord :: Pord
  }
  deriving stock (Eq, Show, Generic)

data Q =
  Q {
    name :: Text
  }
  deriving stock (Eq, Show, Generic)

type Table_Dat0 = UidTable "dat" Int64 Dat0 Prim (Prod '[Delete Prim])

type Table_Dat1 =
  UidTable "dat" Int64 Dat1 Prim (Prod [
    Default "0" (Delete Prim),
    TypeName "Pord" (Prod '[Default "53" Prim])
  ])

type Table_Dat2 =
  UidTable "dat" Int64 Dat2 Prim (Prod [
    Default "15" Prim,
    TypeName "Pord" (Prod [Prim, Nullable Prim])
  ])

type Table_Dat =
  UidTable "dat" Int64 Dat Prim (Prod [
    Default "\"vunqach\"" Prim,
    Rename "number" Prim,
    Prod [Prim, Nullable Prim]
  ])

table_Dat0 :: Sqel Table_Dat0
table_Dat0 = sqel

table_Dat1 :: Sqel Table_Dat1
table_Dat1 = sqel

table_Dat2 :: Sqel Table_Dat2
table_Dat2 = sqel

table_Dat :: Sqel Table_Dat
table_Dat = sqel

migrations :: Migrate Def Identity [Table_Dat, Table_Dat2, Table_Dat1, Table_Dat0]
migrations =
  table_Dat0 --> table_Dat1 --> table_Dat2 --> table_Dat

migrationErrors :: NonEmpty Text
migrationErrors =
  [
    "The migration table 'dat' [version 1] has mismatched columns:",
    " • The column 'number' with type 'bigint' was removed.",
    "The composite type 'sqel_type__pord' [version 2] has mismatched columns:",
    " • The type of the column 'p1' was changed from 'text' to 'bigint'.",
    "The type 'sqel_type__point' [version 2] was removed."
  ]

test_migrationConsistency :: TestT IO ()
test_migrationConsistency = do
  golden <- evalEither (Aeson.eitherDecodeStrict' meta1)
  Left migrationErrors === checkMigrationConsistency golden (migrationsMetadata migrations)
