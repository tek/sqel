module Sqel.Migration.Transform where

import Hasql.Decoders (Row)
import Hasql.Encoders (Params)

import Sqel.Build.Sql (BuildClause)
import Sqel.Class.DefaultFields (DefaultMeta)
import Sqel.Class.MigrationEffect (MigrationEffect (runStatement, runStatement_))
import Sqel.Class.ReifyCodec (ReifyCodec, reifyParams, reifyRow)
import Sqel.Data.Clause (ClauseK (ClauseK))
import Sqel.Data.Codec (Decoder, Encoder)
import Sqel.Data.Dd (DdK)
import Sqel.Data.Migration (
  CompAction,
  Mig (Mig),
  Migration (Migration),
  MigrationActions (CustomActions),
  MigrationVersion,
  )
import Sqel.Data.PgTypeName (PgCompName, pattern PgOnlyTableName)
import Sqel.Data.Sqel (SqelFor)
import Sqel.Data.Sql (sql)
import Sqel.Data.Statement (Statement (Statement))
import Sqel.Dd (DdType)
import Sqel.Default (CreateTable, CreateType, From, InsertInto, Select, Values)
import qualified Sqel.Migration.Data.TransformStep
import Sqel.Migration.Data.TransformStep (TransformStep (TransformStep))
import Sqel.Migration.Data.TypeStatus (TypeStatus (Absent))
import Sqel.Migration.Run (autoKeys, runTypesMigration)
import Sqel.Migration.Table (TableTypeChanges (tableTypeChanges))
import Sqel.Sqel (sqelTableName)
import qualified Sqel.Statement.Common as Statement
import Sqel.Syntax.Result (TableQResult, TableResult)

data MigrateTransform tag m old new =
  MigrateTransform {
    trans :: [old] -> m [new],
    types :: Map PgCompName (CompAction tag),
    decoder :: Row old,
    encoder :: Params new
  }

type TransformAndMigrate :: ∀ {ext} . Type -> (Type -> Type) -> DdK ext -> DdK ext -> Constraint
class TransformAndMigrate tag m old new where
  transformTypeKeys :: Map PgCompName (CompAction tag) -> m (Set (PgCompName, Bool))

  transformAndMigrate ::
    MigrateTransform tag m (DdType old) (DdType new) ->
    TypeStatus ->
    SqelFor tag old ->
    SqelFor tag new ->
    Set PgCompName ->
    m ()

instance (
    Monad m,
    DefaultMeta tag,
    MigrationEffect m,
    BuildClause tag CreateType,
    BuildClause tag Select,
    BuildClause tag From,
    TableResult (Statement () ()) tag new '[ 'ClauseK CreateTable],
    TableQResult (Statement (DdType new) ()) tag new ['ClauseK InsertInto, 'ClauseK Values]
  ) => TransformAndMigrate tag m old new where
    transformTypeKeys types = pure (autoKeys types)

    transformAndMigrate MigrateTransform {types} Absent _ _ eligible =
      runTypesMigration eligible types

    transformAndMigrate MigrateTransform {..} _ current new eligible = do
      oldRows <- runStatement () (Statement.selectAll current)
      newRows <- trans oldRows
      runTypesMigration eligible types
      runStatement_ () (Statement [sql|alter table "##{oldName}" rename to "##{oldName}-migration-temp"|] mempty unit)
      runStatement_ () (Statement.createTable new)
      for_ newRows (flip runStatement_ (Statement.insert new))
      where
        PgOnlyTableName oldName = sqelTableName current

type MkMigrateTransform :: ∀ {ext} . Type -> (Type -> Type) -> DdK ext -> DdK ext -> Constraint
class MkMigrateTransform tag m old new where
  migrateTransform ::
    MigrationVersion ->
    SqelFor tag old ->
    SqelFor tag new ->
    ([DdType old] -> m [DdType new]) ->
    Migration tag m ('Mig old new)

instance (
    TableTypeChanges tag old new,
    TransformAndMigrate tag m old new,
    ReifyCodec Decoder old,
    ReifyCodec Encoder new
  ) => MkMigrateTransform tag m old new where
    migrateTransform version old new f =
      Migration version old new custom
      where
        custom = CustomActions (transformTypeKeys @tag @m @old @new types) (transformAndMigrate mt)
        mt =
          MigrateTransform {
            trans = f,
            types = types,
            decoder = reifyRow @old,
            encoder = reifyParams @new
          }
        types = tableTypeChanges old new

transform ::
  ([old] -> m [DdType new]) ->
  SqelFor tag new ->
  TransformStep tag m old new
transform trans new =
  TransformStep {..}
