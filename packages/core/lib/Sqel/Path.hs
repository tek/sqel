module Sqel.Path where

import qualified Data.List.NonEmpty as NonEmpty

import Sqel.Data.Path (FieldPath (FieldPath), PrimPath (PrimPath))
import Sqel.Data.PgTypeName (pattern PgOnlyTableName, PgTableName)
import Sqel.Data.Selector (Selector)
import Sqel.Data.Sql (Sql, toSql)
import qualified Sqel.Default
import Sqel.Default (PrimMeta)
import Sqel.SOP.Constraint (KnownSymbols, symbolTexts)
import Sqel.Selector (joinSelector, nameSelector, typeSelector, typeSelectorTable)

ddPath ::
  ∀ path .
  KnownSymbols path =>
  FieldPath
ddPath =
  FieldPath (NonEmpty.reverse (symbolTexts @path))

primPath :: Bool -> PgTableName -> FieldPath -> PrimPath
primPath multi table path =
  PrimPath mayTable path
  where
    mayTable | multi = Just table
             | otherwise = Nothing

primMetaPath :: Bool -> PrimMeta -> PrimPath
primMetaPath multi meta =
  primPath multi meta.table meta.path

primSelector :: PrimPath -> Selector
primSelector = \case
  PrimPath (Just (PgOnlyTableName table)) [name] ->
    joinSelector @[] [table, name]
  PrimPath (Just (PgOnlyTableName table)) (FieldPath names) ->
    typeSelectorTable table names
  PrimPath Nothing [name] ->
    nameSelector name
  PrimPath Nothing (FieldPath (root :| names)) ->
    typeSelector root names

renderPrimPath :: PrimPath -> Sql
renderPrimPath = toSql . primSelector
