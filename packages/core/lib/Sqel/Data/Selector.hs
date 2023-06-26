module Sqel.Data.Selector where

import Exon (exon)
import qualified Exon.Combinators as Exon
import Prettyprinter (Pretty (pretty))

import Sqel.Data.PgTypeName (pattern PgOnlyTableName, PgTableName)
import Sqel.Data.Sql (Sql (Sql), ToSql (toSql), sql)
import Sqel.Text.DbIdentifier (quotedDbId)

newtype Selector =
  Selector { unSelector :: Sql }
  deriving stock (Eq, Show, Generic, Ord)
  deriving newtype (IsString, Semigroup, Monoid)

instance Pretty Selector where
  pretty (Selector s) = pretty s

textSelector :: Text -> Selector
textSelector =
  Selector . Sql

nameSelector :: Text -> Selector
nameSelector =
  textSelector . quotedDbId

assign :: Selector -> Sql -> Sql
assign (Selector name) value =
  [sql|#{name} = #{value}|]

instance ToSql Selector where
  toSql = (.unSelector)

joinSelector ::
  Functor t =>
  Foldable t =>
  t Text ->
  Selector
joinSelector =
  textSelector . Exon.intercalate "." . fmap quotedDbId

typeSelector ::
  Functor t =>
  Foldable t =>
  Text ->
  t Text ->
  Selector
typeSelector root path =
  [exon|(##{quotedDbId root}).#{joinSelector path}|]

typeSelectorTable ::
  Text ->
  NonEmpty Text ->
  Selector
typeSelectorTable table (root :| path) =
  [exon|(##{quotedDbId table}.##{quotedDbId root}).#{joinSelector path}|]

pathSelector :: NonEmpty Text -> Selector
pathSelector = \case
  [n] -> textSelector (quotedDbId n)
  (root :| path) -> typeSelector root path

pathSelectorTable :: Maybe PgTableName -> NonEmpty Text -> Selector
pathSelectorTable alias path =
  case (alias, path) of
    (Just (PgOnlyTableName table), [name]) ->
      joinSelector @[] [table, name]
    (Just (PgOnlyTableName table), names) ->
      typeSelectorTable table names
    (Nothing, [name]) ->
      nameSelector name
    (Nothing, root :| names) ->
      typeSelector root names
