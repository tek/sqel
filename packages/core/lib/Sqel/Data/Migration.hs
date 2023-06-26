module Sqel.Data.Migration where

import Data.Aeson (FromJSON, ToJSON)
import Exon (exon)

import Sqel.Data.Dd (DdK (Dd), StructWith (Comp))
import Sqel.Data.PgType (PgColumnName)
import Sqel.Data.PgTypeName (PgCompName, PgTypeName)
import Sqel.Data.Sqel (SqelFor)
import Sqel.Migration.Data.TypeStatus (TypeStatus)

data ColumnAction tag where
  AddColumn :: SqelFor tag col -> PgColumnName -> Maybe Text -> ColumnAction tag
  RemoveColumn :: PgColumnName -> ColumnAction tag
  RenameColumn :: PgColumnName -> PgColumnName -> ColumnAction tag
  RenameColumnType :: PgColumnName -> PgCompName -> ColumnAction tag

instance Show (ColumnAction tag) where
  showsPrec d =
    showParen (d > 10) . \case
      AddColumn _ name _ ->
        [exon|AddColumn #{showsPrec 11 name}|]
      RemoveColumn name ->
        [exon|RemoveColumn #{showsPrec 11 name}|]
      RenameColumn old new ->
        [exon|RenameColumn #{showsPrec 11 old} #{showsPrec 11 new}|]
      RenameColumnType name new ->
        [exon|RenameColumnType #{showsPrec 11 name} #{showsPrec 11 new}|]

-- TODO type SomeComp that enforces the Comp kind for Sqel
type TypeAction :: Type -> Bool -> Type
data TypeAction tag table where
  ModifyAction :: PgTypeName table -> [ColumnAction tag] -> TypeAction tag table
  RenameAction :: PgCompName -> [ColumnAction tag] -> TypeAction tag 'False
  AddAction :: SqelFor tag ('Dd ext a ('Comp tsel c i sub)) -> TypeAction tag 'False

type TableAction tag = TypeAction tag 'True
type CompAction tag = TypeAction tag 'False

type Mig :: Type -> Type
data Mig ext =
  Mig {
    from :: DdK ext,
    to :: DdK ext
  }

type MigrationActions :: ∀ {ext} . Type -> (Type -> Type) -> Mig ext -> Type
data MigrationActions tag m mig where

  AutoActions :: {
    table :: TableAction tag,
    types :: Map PgCompName (CompAction tag)
  } -> MigrationActions tag m ('Mig old new)

  CustomActions :: {
      typeKeys :: m (Set (PgCompName, Bool)),
      action :: TypeStatus -> SqelFor tag old -> SqelFor tag new -> Set PgCompName -> m ()
    } -> MigrationActions tag m ('Mig old new)

newtype MigrationVersion =
  MigrationVersion Word
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord, FromJSON, ToJSON)

type Migration :: ∀ {ext} . Type -> (Type -> Type) -> Mig ext -> Type
data Migration tag m mig where
  Migration :: {
    version :: MigrationVersion,
    tableFrom :: SqelFor tag from,
    tableTo :: SqelFor tag to,
    actions :: MigrationActions tag m ('Mig from to)
  } -> Migration tag m ('Mig from to)

type MigFrom :: ∀ {ext} . Mig ext -> DdK ext
type family MigFrom mig where
  MigFrom ('Mig from _) = from

type MigTo :: ∀ {ext} . Mig ext -> DdK ext
type family MigTo mig where
  MigTo ('Mig _ to) = to

type UniMigs :: DdK ext -> [DdK ext] -> [Mig ext]
type family UniMigs new tables = r | r -> tables where
  UniMigs _ '[] = '[]
  UniMigs new (old : tables) = 'Mig old new : UniMigs old tables

type Migrations :: Type -> (Type -> Type) -> [Mig ext] -> Type
data Migrations tag m migs where
  Migrations ::
    Migration tag m ('Mig from to) ->
    Migrations tag m ('Mig from' from : migs) ->
    Migrations tag m ('Mig from to : 'Mig from' from : migs)
  InitialMigration :: Migration tag m ('Mig from to) -> Migrations tag m '[ 'Mig from to]

type TableDdl :: ∀ {ext} . Type -> (Type -> Type) -> DdK ext -> [Mig ext] -> Type
data TableDdl tag m table migs where
  TableDdl :: SqelFor tag table -> TableDdl tag m table '[]
  TableMigrations ::
    SqelFor tag table ->
    Migrations tag m ('Mig from table : migs) ->
    TableDdl tag m table ('Mig from table : migs)

tableDdlCurrent :: TableDdl tag m table migs -> SqelFor tag table
tableDdlCurrent = \case
  TableDdl table -> table
  TableMigrations table _ -> table

type Migrate :: Type -> (Type -> Type) -> [DdK ext] -> Type
type family Migrate tag m tables where
  Migrate tag m (cur : old) = TableDdl tag m cur (UniMigs cur old)

noMigrations :: SqelFor tag table -> TableDdl tag m table '[]
noMigrations = TableDdl

latestMigrationVersion :: Migrations tag m (migs) -> MigrationVersion
latestMigrationVersion = \case
  Migrations mig _ -> mig.version
  InitialMigration mig -> mig.version
