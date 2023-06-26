module Sqel.Migration.Data.MigrationMetadata where

import Data.Aeson (FromJSON, ToJSON)

import Sqel.Class.TransformMeta (Transform, TransformMeta, transform)
import Sqel.Data.Migration (MigrationVersion (MigrationVersion))
import Sqel.Data.PgTypeName (PgTableName)
import Sqel.Data.Spine (CompFor, PrimFor, Spine, TypeSpine)
import Sqel.Data.Sql (Sql)

data MigrationMetadata tag =
  MigrationMetadata {
    name :: PgTableName,
    version :: MigrationVersion,
    table :: [Spine tag],
    types :: [TypeSpine tag],
    statementsTable :: [Sql],
    statementsMigration :: [Sql]
  }
  deriving stock (Generic)

deriving stock instance (Eq (CompFor tag), Eq (PrimFor tag)) => Eq (MigrationMetadata tag)
deriving stock instance (Show (CompFor tag), Show (PrimFor tag)) => Show (MigrationMetadata tag)
deriving anyclass instance (ToJSON (CompFor tag), ToJSON (PrimFor tag)) => ToJSON (MigrationMetadata tag)
deriving anyclass instance (FromJSON (CompFor tag), FromJSON (PrimFor tag)) => FromJSON (MigrationMetadata tag)

instance TransformMeta tag1 tag2 => Transform tag1 tag2 MigrationMetadata where
  transform MigrationMetadata {..} =
    MigrationMetadata {table = transform <$> table, types = transform <$> types, ..}

showVersion :: MigrationMetadata tag -> Text
showVersion MigrationMetadata {version = MigrationVersion v} = show v
