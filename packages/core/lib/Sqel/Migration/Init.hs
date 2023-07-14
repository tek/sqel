module Sqel.Migration.Init where

import Data.Some (withSome)
import Exon (exon)
import Generics.SOP (All, hctraverse_)

import Sqel.Build.Sql (BuildClause)
import Sqel.Class.DefaultFields (DefaultMeta (defaultCompMeta))
import qualified Sqel.Class.MigrationEffect as MigrationEffect
import Sqel.Class.MigrationEffect (MigrationEffect)
import Sqel.Data.Dd (Dd (Dd), Struct (Comp, Prim))
import Sqel.Data.PgTypeName (pattern PgOnlyTableName, getPgTypeName)
import Sqel.Data.Sqel (SqelFor (SqelPrim), pattern SqelMerge, pattern SqelNest)
import Sqel.Dd (DdSub)
import qualified Sqel.Default
import Sqel.Default (CreateTable, CreateType)
import Sqel.Migration.Metadata (DbCols (DbCols), typeColumns)
import Sqel.Sqel (sqelCompName, sqelTableName)
import qualified Sqel.Statement.Common as Sqel
import Sqel.Statement.PgSchema (typeColumnsSql)

compNeedsInit ::
  ∀ tag s m .
  Monad m =>
  DefaultMeta tag =>
  MigrationEffect m =>
  SqelFor tag s ->
  m Bool
compNeedsInit = \case
  SqelPrim _ _ _ -> pure False
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
  SqelPrim _ _ _ -> unit
  s@(SqelNest _ _ _ _) -> do
    MigrationEffect.log [exon|Initializing type '#{getPgTypeName (sqelCompName s)}'|]
    MigrationEffect.runStatement_ () (Sqel.createType s)
  SqelMerge _ _ _ _ -> unit

type InitComp :: ∀ {ext} . Type -> Dd ext -> Constraint
class InitComp tag s where
  initComp :: Monad m => MigrationEffect m => SqelFor tag s -> m ()

instance (
    DefaultMeta tag,
    All (InitComp tag) sub,
    BuildClause tag CreateType
  ) => InitComp tag ('Dd ext a ('Comp tsel c i sub)) where
    initComp s =
      whenM (compNeedsInit s) do
        initStructure s
        createType s

instance InitComp tag ('Dd ext a ('Prim prim)) where
  initComp _ = unit

initStructure ::
  ∀ tag m s .
  Monad m =>
  MigrationEffect m =>
  All (InitComp tag) (DdSub s) =>
  SqelFor tag s ->
  m ()
initStructure = \case
  SqelNest _ _ cols _ -> hctraverse_ (Proxy @(InitComp tag)) initComp cols
  SqelMerge _ _ cols _ -> hctraverse_ (Proxy @(InitComp tag)) initComp cols
  SqelPrim _ _ _ -> unit

type InitTable :: ∀ {ext} . Type -> Dd ext -> Constraint
class InitTable tag table where
  initTable :: Monad m => MigrationEffect m => SqelFor tag table -> m ()

instance (
    DefaultMeta tag,
    All (InitComp tag) (DdSub table),
    BuildClause tag CreateTable
  ) => InitTable tag table where
  initTable table = do
    MigrationEffect.log [exon|Initializing table '#{name}'|]
    initStructure table
    MigrationEffect.runStatement_ () (Sqel.createTable table)
    where
      PgOnlyTableName name = sqelTableName table
