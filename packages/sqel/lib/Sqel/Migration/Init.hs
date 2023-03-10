module Sqel.Migration.Init where

import Exon (exon)
import Lens.Micro ((^.))

import qualified Sqel.Class.MigrationEffect as MigrationEffect
import Sqel.Class.MigrationEffect (MigrationEffect)
import Sqel.Data.PgType (
  PgColumnName (PgColumnName),
  PgComposite (PgComposite),
  PgStructure (PgStructure),
  PgTable,
  StructureType (StructureComp, StructurePrim),
  structureToColumns,
  )
import Sqel.Data.PgTypeName (PgCompName, getPgTypeName)
import Sqel.Migration.Metadata (DbCols (DbCols), typeColumns)
import qualified Sqel.Sql.Type as Sql
import Sqel.Statement (createTable, plain, typeColumnsSql)

initComp ::
  Monad m =>
  MigrationEffect m =>
  PgCompName ->
  PgStructure ->
  m ()
initComp tpe structure = do
  DbCols existing <- typeColumns typeColumnsSql tpe
  when (null existing) createType
  where
    createType = do
      initStructure structure
      MigrationEffect.runStatement_ () (plain (Sql.createProdType (PgComposite tpe (structureToColumns structure))))

initType ::
  Monad m =>
  MigrationEffect m =>
  PgColumnName ->
  StructureType ->
  m ()
initType (PgColumnName _) = \case
  StructurePrim _ _ ->
    unit
  StructureComp tpe columns _ ->
    initComp tpe columns

initStructure ::
  Monad m =>
  MigrationEffect m =>
  PgStructure ->
  m ()
initStructure (PgStructure cols) =
  traverse_ (uncurry initType) cols

initTable ::
  Monad m =>
  MigrationEffect m =>
  PgTable a ->
  m ()
initTable table = do
  MigrationEffect.log [exon|Initializing table '#{getPgTypeName (table ^. #name)}'|]
  initStructure (table ^. #structure)
  MigrationEffect.runStatement_ () (createTable table)
