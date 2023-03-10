module Sqel.Data.PgType where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map.Strict as Map
import Lens.Micro.Extras (view)
import Prettyprinter (Pretty (pretty), nest, sep, vsep, (<+>))

import qualified Sqel.ColumnConstraints as ColumnConstraints
import Sqel.ColumnConstraints (Constraints (Constraints))
import Sqel.Data.PgTypeName (PgCompName, PgTableName, pattern PgTypeName)
import Sqel.Data.Selector (Selector (unSelector), assign, nameSelector)
import Sqel.Data.Sql (Sql, ToSql (toSql), sql, sqlQuote)
import Sqel.Data.SqlFragment (
  CommaSep (CommaSep),
  Create (Create),
  Delete (Delete),
  From (From),
  Insert (Insert),
  Into (Into),
  Returning (Returning),
  Select (Select),
  Update (Update),
  UpdateSet (UpdateSet),
  )
import Sqel.SOP.Constraint (symbolText)
import Sqel.Text.DbIdentifier (dbIdentifierT, dbSymbol)

newtype PgPrimName =
  PgPrimName { unPgPrimName :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, Semigroup, Monoid, ToJSON, FromJSON)

instance Pretty PgPrimName where
  pretty (PgPrimName n) = pretty n

pgPrimName ::
  ∀ name .
  KnownSymbol name =>
  PgPrimName
pgPrimName =
  PgPrimName (dbSymbol @name)

newtype PgProdName =
  PgProdName { unPgProdName :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

newtype PgColumnName =
  PgColumnName { unPgColumnName :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Ord, ToJSON, FromJSON)

instance Pretty PgColumnName where
  pretty (PgColumnName n) = pretty n

instance ToSql PgColumnName where
  toSql =
    sqlQuote . unPgColumnName

pgColumnName ::
  Text ->
  PgColumnName
pgColumnName n =
  PgColumnName (dbIdentifierT n)

instance IsString PgColumnName where
  fromString = pgColumnName . fromString

newtype PgTypeRef =
  PgTypeRef { unPgTypeRef :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, ToJSON, FromJSON)

instance Pretty PgTypeRef where
  pretty (PgTypeRef n) = pretty n

instance ToSql PgTypeRef where
  toSql = sqlQuote . unPgTypeRef

pgTypeRef ::
  Text ->
  PgTypeRef
pgTypeRef n =
  PgTypeRef (dbIdentifierT n)

pgCompRef :: PgCompName -> PgTypeRef
pgCompRef (PgTypeName n) =
  PgTypeRef n

pgTypeRefSym ::
  ∀ tname .
  KnownSymbol tname =>
  PgTypeRef
pgTypeRefSym =
  pgTypeRef (symbolText @tname)

data ColumnType =
  ColumnPrim { name :: PgPrimName, constraints :: Constraints }
  |
  ColumnComp { pgType :: PgTypeRef, constraints :: Constraints }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data PgColumn =
  PgColumn {
    name :: PgColumnName,
    pgType :: ColumnType
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Pretty PgColumn where
  pretty = \case
    PgColumn n (ColumnPrim t Constraints {fragments}) -> "*" <+> pretty n <+> pretty t <+> sep (pretty <$> fragments)
    PgColumn n (ColumnComp t Constraints {fragments}) -> "+" <+> pretty n <+> pretty t <+> sep (pretty <$> fragments)

instance ToSql (Create PgColumn) where
  toSql (Create PgColumn {..}) =
    case pgType of
      ColumnPrim (PgPrimName tpe) constr ->
        [sql|##{name} ##{tpe} ##{constr}|]
      ColumnComp (PgTypeRef tpe) constr ->
        [sql|##{name} ##{tpe} ##{constr}|]

newtype PgColumns =
  PgColumns { unPgColumns :: [PgColumn] }
  deriving stock (Eq, Show)
  deriving newtype (FromJSON, ToJSON)

data StructureType =
  StructurePrim { name :: PgPrimName, constraints :: Constraints }
  |
  StructureComp { compName :: PgCompName, struct :: PgStructure, constraints :: Constraints }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

structureToColumn :: StructureType -> ColumnType
structureToColumn = \case
  StructurePrim {..} -> ColumnPrim {..}
  StructureComp (PgTypeName ref) _ constr -> ColumnComp (PgTypeRef ref) constr

instance Pretty PgColumns where
  pretty (PgColumns cs) =
    vsep (pretty <$> cs)

instance ToSql (CommaSep PgColumns) where
  toSql (CommaSep (PgColumns cols)) =
    toSql (CommaSep (view #name <$> cols))

instance ToSql (Create PgColumns) where
  toSql (Create (PgColumns cols)) =
    [sql|(##{CommaSep (Create <$> cols)})|]

newtype PgStructure =
  PgStructure { unPgColumns :: [(PgColumnName, StructureType)] }
  deriving stock (Eq, Show)
  deriving newtype (FromJSON, ToJSON)

structureToColumns :: PgStructure -> PgColumns
structureToColumns (PgStructure cols) =
  PgColumns (uncurry PgColumn . second structureToColumn <$> cols)

data PgComposite =
  PgComposite {
    name :: PgCompName,
    columns :: PgColumns
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Pretty PgComposite where
  pretty PgComposite {..} =
    nest 2 (vsep ["type" <+> pretty name, pretty columns])

newtype TableSelectors =
  TableSelectors { unTableSelectors :: [Selector] }
  deriving stock (Eq, Show, Generic)

instance ToSql (CommaSep TableSelectors) where
  toSql (CommaSep (TableSelectors s)) =
    toSql (CommaSep (unSelector <$> s))

instance ToSql (Select TableSelectors) where
  toSql (Select s) =
    "select " <> toSql (CommaSep s)

newtype TableValues =
  TableValues { unTableValues :: [Sql] }
  deriving stock (Eq, Show, Generic)

data PgTable a =
  PgTable {
    name :: PgTableName,
    columns :: PgColumns,
    types :: Map PgTypeRef PgComposite,
    selectors :: TableSelectors,
    values :: TableValues,
    structure :: PgStructure
  }
  deriving stock (Show, Generic)

instance Pretty (PgTable a) where
  pretty PgTable {..} =
    nest 2 (vsep (("table" <+> pretty name) : pretty columns : (pretty <$> Map.elems types)))

instance ToSql (Create (PgTable a)) where
  toSql (Create PgTable {name, columns}) =
    [sql|create table ##{name} ##{Create columns}|]

instance ToSql (Select (PgTable a)) where
  toSql (Select PgTable {name, selectors}) =
    [sql|##{Select selectors} ##{From name}|]

instance ToSql (Update (PgTable a)) where
  toSql (Update PgTable {name}) =
    [sql|update ##{name}|]

instance ToSql (UpdateSet (PgTable a)) where
  toSql (UpdateSet PgTable {columns = PgColumns columns, values = TableValues values}) =
    [sql|update set ##{CommaSep assigns}|]
    where
      assigns = zipWith assign colNames values
      colNames = columns <&> \ (PgColumn (PgColumnName name) _) -> nameSelector name

instance ToSql (Returning (PgTable a)) where
  toSql (Returning (PgTable {selectors})) =
    [sql|returning ##{CommaSep selectors}|]

instance ToSql (Insert (PgTable a)) where
  toSql (Insert PgTable {name, columns, values = TableValues values}) =
    [sql|insert ##{Into name} (##{CommaSep columns}) values (##{CommaSep values})|]

instance ToSql (Delete (PgTable a)) where
  toSql (Delete PgTable {name}) =
    [sql|delete ##{From name}|]
