module Sqel.Migration.Init where

import Data.Some (withSome)
import Exon (exon)
import Generics.SOP (All, hctraverse_)

import Sqel.Build.Sql (BuildClause)
import Sqel.Class.DefaultFields (DefaultMeta (defaultCompMeta))
import qualified Sqel.Class.MigrationEffect as MigrationEffect
import Sqel.Class.MigrationEffect (MigrationEffect)
import Sqel.Data.Clause (ClauseK (ClauseK))
import Sqel.Data.Dd (DdK (Dd), StructWith (Comp, Prim))
import Sqel.Data.PgTypeName (pattern PgOnlyTableName, getPgTypeName)
import Sqel.Data.Sqel (SqelFor (SqelPrim), pattern SqelMerge, pattern SqelNest)
import Sqel.Data.Statement (Statement)
import Sqel.Dd (DdSub)
import qualified Sqel.Default
import Sqel.Default (CreateTable, CreateType)
import Sqel.Migration.Metadata (DbCols (DbCols), typeColumns)
import Sqel.Sqel (sqelCompName, sqelTableName)
import qualified Sqel.Statement.Common as Sqel
import Sqel.Statement.PgSchema (typeColumnsSql)
import Sqel.Syntax.Result (TableResult)

compNeedsInit ::
  ∀ tag s m .
  Monad m =>
  DefaultMeta tag =>
  MigrationEffect m =>
  SqelFor tag s ->
  m Bool
compNeedsInit = \case
  SqelPrim _ _ -> pure False
  SqelNest (defaultCompMeta @tag -> meta) _ _ _ ->
    withSome meta.typeName \ name -> do
      DbCols existing <- typeColumns typeColumnsSql name
      pure (null existing)
  SqelMerge _ _ _ _ -> pure True

createType ::
  Monad m =>
  DefaultMeta tag =>
  MigrationEffect m =>
  BuildClause tag CreateType =>
  SqelFor tag s ->
  m ()
createType = \case
  SqelPrim _ _ -> unit
  s@(SqelNest _ _ _ _) -> do
    MigrationEffect.log [exon|Initializing type '#{getPgTypeName (sqelCompName s)}'|]
    MigrationEffect.runStatement_ () (Sqel.createType s)
  SqelMerge _ _ _ _ -> unit

type InitComp :: ∀ {ext} . Type -> (Type -> Type) -> DdK ext -> Constraint
class InitComp tag m s where
  initComp :: SqelFor tag s -> m ()

instance (
    Monad m,
    DefaultMeta tag,
    MigrationEffect m,
    All (InitComp tag m) sub
  ) => InitComp tag m ('Dd ext a ('Comp tsel c i sub)) where
    initComp s =
      whenM (compNeedsInit s) do
        initStructure s
        createType s

instance Applicative m => InitComp tag m ('Dd ext a ('Prim prim)) where
  initComp _ = unit

initStructure ::
  ∀ tag m s .
  Monad m =>
  All (InitComp tag m) (DdSub s) =>
  SqelFor tag s ->
  m ()
initStructure = \case
  SqelNest _ _ cols _ -> hctraverse_ (Proxy @(InitComp tag m)) initComp cols
  SqelMerge _ _ cols _ -> hctraverse_ (Proxy @(InitComp tag m)) initComp cols
  SqelPrim _ _ -> unit

type InitTable :: ∀ {ext} . Type -> (Type -> Type) -> DdK ext -> Constraint
class InitTable tag m table where
  initTable :: SqelFor tag table -> m ()

instance (
    Monad m,
    DefaultMeta tag,
    MigrationEffect m,
    All (InitComp tag m) (DdSub table),
    TableResult (Statement () ()) tag table '[ 'ClauseK CreateTable]
  ) => InitTable tag m table where
  initTable table = do
    MigrationEffect.log [exon|Initializing table '#{name}'|]
    initStructure table
    MigrationEffect.runStatement_ () (Sqel.createTable table)
    where
      PgOnlyTableName name = sqelTableName table