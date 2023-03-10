module Sqel.PgType where

import Data.List.NonEmpty ((<|))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Exon
import Exon (exon)
import Lens.Micro (_1, _2, _3, _4, (^.))
import Lens.Micro.Extras (view)
import Type.Errors (ErrorMessage)

import Sqel.Class.MatchView (MatchProjection)
import Sqel.Data.Codec (Codec (Codec), FullCodec)
import Sqel.Data.Dd (Dd, DdK, DdType)
import Sqel.Data.PgType (
  ColumnType (ColumnComp, ColumnPrim),
  PgColumn (PgColumn),
  PgColumnName (PgColumnName),
  PgColumns (PgColumns),
  PgComposite (PgComposite),
  PgStructure (PgStructure),
  PgTable (PgTable),
  PgTypeRef,
  StructureType (StructureComp, StructurePrim),
  TableSelectors (TableSelectors),
  TableValues (TableValues),
  pgCompRef,
  )
import Sqel.Data.PgTypeName (PgTableName, pgCompName, pgTableName)
import qualified Sqel.Data.Projection as Projection
import Sqel.Data.Projection (Projection (Projection))
import Sqel.Data.ProjectionWitness (ProjectionWitness (ProjectionWitness))
import Sqel.Data.Selector (Selector (Selector))
import Sqel.Data.Sql (Sql (Sql), sql)
import qualified Sqel.Data.TableSchema as TableSchema
import Sqel.Data.TableSchema (TableSchema (TableSchema))
import Sqel.Data.Term (Comp, CompInc (Merge), DdTerm (DdTerm), Struct (Comp, Prim))
import Sqel.ReifyCodec (ReifyCodec (reifyCodec))
import Sqel.ReifyDd (ReifyDd (reifyDd))
import Sqel.SOP.Error (Quoted)
import Sqel.Sql.Prepared (dollar)
import Sqel.Text.Quote (dquote)

pgColumn ::
  DdTerm ->
  ([PgColumn], [(PgColumnName, StructureType)], Map PgTypeRef PgComposite, [NonEmpty PgColumnName])
pgColumn = \case
  DdTerm name _ constr (Prim t) ->
    ([PgColumn name (ColumnPrim t constr)], [(name, StructurePrim t constr)], mempty, [pure name])
  DdTerm name _ constr (Comp typeName c i sub) ->
    case comp typeName c i sub of
      (compType@(PgComposite cname _), struct, types, False, sels) ->
        (colType, structType, Map.insert ref compType types, (name <|) <$> sels)
        where
          colType = [PgColumn name (ColumnComp ref constr)]
          structType = [(name, StructureComp cname struct constr)]
          ref = pgCompRef cname
      (PgComposite _ (PgColumns columns), PgStructure struct, types, True, sels) ->
        (columns, struct, types, sels)

comp ::
  Text ->
  Comp ->
  CompInc ->
  [DdTerm] ->
  (PgComposite, PgStructure, Map PgTypeRef PgComposite, Bool, [NonEmpty PgColumnName])
comp typeName _ i sub =
  (compType, structType, Map.unions (view _3 <$> cols), i == Merge, view _4 =<< cols)
  where
    compType = PgComposite compName (PgColumns (view _1 =<< cols))
    structType = PgStructure (view _2 =<< cols)
    compName = pgCompName typeName
    cols = pgColumn <$> sub

-- TODO this used to dquote the @names@ as well but it appears to fail for the sum index field
mkSelector :: NonEmpty PgColumnName -> Selector
mkSelector =
  Selector . Sql . \case
    [PgColumnName name] -> dquote name
    root :| names -> [exon|(##{dquote root}).##{Text.intercalate "." (coerce names)}|]

-- TODO use CommaSep
mkValues :: PgStructure -> [Sql]
mkValues (PgStructure base) =
  snd (mapAccumL mkCol (1 :: Int) base)
  where
    mkCol (n :: Int) = \case
      (_, StructurePrim _ _) -> (n + 1, [sql|##{dollar n}|])
      (_, StructureComp _ (PgStructure cols) _) ->
        (newN, [sql|row(#{Exon.intercalate ", " sub})|])
        where
          (newN, sub) =
            mapAccumL mkCol n cols

mkTable ::
  PgColumnName ->
  Maybe PgTableName ->
  PgColumns ->
  Map PgTypeRef PgComposite ->
  [NonEmpty PgColumnName] ->
  PgStructure ->
  PgTable a
mkTable (PgColumnName name) tableName cols types selectors struct =
  PgTable (fromMaybe (pgTableName name) tableName) cols types (TableSelectors (mkSelector <$> selectors)) values struct
  where
    values = TableValues (mkValues struct)

toTable :: DdTerm -> PgTable a
toTable = \case
  DdTerm name tableName constr (Prim t) ->
    mkTable name tableName cols [] [pure name] struct
    where
      cols = PgColumns [PgColumn name (ColumnPrim t constr)]
      struct = PgStructure [(name, StructurePrim t constr)]
  DdTerm name tableName _ (Comp typeName c i sub) ->
    mkTable name tableName cols types paths struct
    where
      (PgComposite _ cols, struct, types, _, paths) = comp typeName c i sub

pgTable ::
  ??? s .
  ReifyDd s =>
  Dd s ->
  PgTable (DdType s)
pgTable dd =
  toTable (reifyDd dd)

type MkTableSchema :: DdK -> Constraint
class MkTableSchema table where
  tableSchema :: Dd table -> TableSchema (DdType table)

instance (
    ReifyDd table,
    ReifyCodec FullCodec table (DdType table)
  ) => MkTableSchema table where
  tableSchema tab =
    TableSchema (pgTable tab) (row ^. #decodeValue) (params ^. #encodeValue)
    where
      Codec params row = reifyCodec @FullCodec tab

class CheckedProjection' (check :: Maybe Void) (s :: DdK) where
  checkedProjection' :: Dd s -> ProjectionWitness (DdType s) table

instance CheckedProjection' 'Nothing s where
  checkedProjection' _ = ProjectionWitness

class CheckedProjection (proj :: DdK) (table :: DdK) where
  checkedProjection :: Dd proj -> ProjectionWitness (DdType proj) (DdType table)

type CheckProjectionStuck :: ErrorMessage
type CheckProjectionStuck =
  "Could not validate projection fields since there is not enough type information available." %
  "You are most likely missing a constraint for " <> Quoted "CheckedProjection" <> "."

instance (
    MatchProjection proj table match,
    CheckedProjection' match proj
  ) => CheckedProjection proj table where
    checkedProjection = checkedProjection' @match

-- TODO check that the table name matches, otherwise a query using the projection will use the wrong name.
-- also possible to automatically set it, but that might be incompatible with the db view interpreter feature, since
-- the name there can't be propagated here. but it would be possible to check only there and do it automatically here.
projectionWitness ::
  ??? proj table .
  CheckedProjection proj table =>
  Dd proj ->
  Dd table ->
  ProjectionWitness (DdType proj) (DdType table)
projectionWitness proj _ =
  checkedProjection @proj @table proj

projection ::
  MkTableSchema proj =>
  MkTableSchema table =>
  CheckedProjection proj table =>
  Dd proj ->
  Dd table ->
  Projection (DdType proj) (DdType table)
projection ddProj ddTable =
  Projection {..}
  where
    table = tableSchema ddTable
    TableSchema {..} = tableSchema ddProj
    witness = projectionWitness ddProj ddTable

fullProjection ::
  MkTableSchema table =>
  CheckedProjection table table =>
  Dd table ->
  Projection (DdType table) (DdType table)
fullProjection dd =
  projection dd dd

toFullProjection :: TableSchema table -> Projection table table
toFullProjection table@TableSchema {..} =
  Projection {table, witness = ProjectionWitness, ..}
