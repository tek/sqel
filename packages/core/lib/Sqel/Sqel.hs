module Sqel.Sqel where

import Data.Some (Some (Some))
import Generics.SOP (NP, SListI)

import Sqel.Class.DefaultFields (DefaultMeta (defaultCompMeta, defaultPrimMeta))
import Sqel.Class.ReifySqel (ReifySqelFor, sqel)
import Sqel.Data.Dd (Dd)
import Sqel.Data.PgType (PgColumnName (PgColumnName))
import Sqel.Data.PgTypeName (PgCompName, PgTableName, pattern PgTypeName, pgCompName, pgTableName)
import Sqel.Data.Spine (Types)
import Sqel.Data.Sqel (SqelFor (SqelPrim), pattern SqelMerge, pattern SqelNest, sqelSpine)
import Sqel.Dd (EmptyQuery, EmptyResult, IsComp)
import qualified Sqel.Default
import Sqel.Default (CompMeta (CompMeta), PrimMeta (PrimMeta))
import Sqel.SOP.NP (hfoldMap)
import Sqel.Spine (spineTypes)

sqelTypes ::
  DefaultMeta tag =>
  SqelFor tag s ->
  Types tag
sqelTypes =
  spineTypes . sqelSpine

emptyQuery ::
  ∀ tag .
  ReifySqelFor tag EmptyQuery =>
  SqelFor tag EmptyQuery
emptyQuery = sqel

emptyResult ::
  ∀ tag .
  ReifySqelFor tag EmptyResult =>
  SqelFor tag EmptyResult
emptyResult = sqel

sqelTableName ::
  ∀ tag s .
  DefaultMeta tag =>
  SqelFor tag s ->
  PgTableName
sqelTableName = \case
  SqelNest (defaultCompMeta @tag -> CompMeta {typeName = Some (PgTypeName name)}) _ _ _ -> pgTableName name
  SqelMerge (defaultCompMeta @tag -> CompMeta {typeName = Some (PgTypeName name)}) _ _ _ -> pgTableName name
  SqelPrim (defaultPrimMeta @tag -> PrimMeta {name = PgColumnName name}) _ -> pgTableName name

sqelCompName ::
  ∀ tag s .
  DefaultMeta tag =>
  SqelFor tag s ->
  PgCompName
sqelCompName = \case
  SqelNest (defaultCompMeta @tag -> CompMeta {typeName = Some (PgTypeName name)}) _ _ _ -> pgCompName name
  SqelMerge (defaultCompMeta @tag -> CompMeta {typeName = Some (PgTypeName name)}) _ _ _ -> pgCompName name
  SqelPrim (defaultPrimMeta @tag -> PrimMeta {name = PgColumnName name}) _ -> pgCompName name

foldTypes ::
  ∀ tag ext (table :: Dd ext) m .
  Monoid m =>
  (∀ (s :: Dd ext) . IsComp s ~ 'True => SqelFor tag s -> m) ->
  SqelFor tag table ->
  m
foldTypes f = \case
  SqelPrim {} -> mempty
  SqelNest _ _ sub _ -> spin sub
  SqelMerge _ _ sub _ -> spin sub
  where
    spin :: ∀ (sub :: [Dd ext]) . SListI sub => NP (SqelFor tag) sub -> m
    spin = hfoldMap one
    one :: ∀ (s :: Dd ext) . SqelFor tag s -> m
    one = \case
      SqelPrim {} -> mempty
      s@(SqelNest _ _ sub _) -> f s <> spin sub
      SqelMerge _ _ sub _ -> spin sub
