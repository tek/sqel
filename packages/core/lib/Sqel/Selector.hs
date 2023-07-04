module Sqel.Selector where

import qualified Exon
import Exon (exon)

import Sqel.Data.Selector (Selector (Selector))
import Sqel.Data.Sql (Sql (Sql))
import Sqel.Text.DbIdentifier (quotedDbId)

textSelector :: Text -> Selector
textSelector =
  Selector . Sql

nameSelector :: Text -> Selector
nameSelector =
  textSelector . quotedDbId

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
