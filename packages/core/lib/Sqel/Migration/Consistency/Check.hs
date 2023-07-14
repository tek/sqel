module Sqel.Migration.Consistency.Check where

import Data.Aeson (FromJSON, ToJSON)
import Data.List.Extra (zipWithLongest)
import Exon (exon)

import Sqel.Class.TransformMeta (TransformMeta (transformCompMeta, transformPrimMeta, transformedSpineComp), transform)
import Sqel.Data.Migration (MigrationVersion (MigrationVersion))
import Sqel.Data.PgType (PgColumnName, PgPrimName (PgPrimName), PgTypeRef (PgTypeRef))
import Sqel.Data.PgTypeName (pattern PgTypeName)
import qualified Sqel.Data.Spine
import Sqel.Data.Spine (
  CompFor,
  PrimFor,
  Spine (SpineMerge, SpineNest, SpinePrim),
  pattern SpineComp,
  TypeSpine (TypeSpine),
  )
import qualified Sqel.Default
import Sqel.Default (CompMeta (CompMeta), Def, PrimMeta (PrimMeta))
import qualified Sqel.Migration.Data.MigrationMetadata
import Sqel.Migration.Data.MigrationMetadata (MigrationMetadata, showVersion)
import Sqel.Text.Quote (squote)

data CMC = CMC

data CMCPrim =
  CMCPrim {
    name :: PgColumnName,
    colType :: PgPrimName
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

type instance PrimFor CMC = CMCPrim

data CMCComp =
  CMCComp {
    name :: PgColumnName,
    colType :: PgTypeRef
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

type instance CompFor CMC = CMCComp

instance TransformMeta Def CMC where
  transformPrimMeta PrimMeta {..} = CMCPrim {..}
  transformCompMeta CompMeta {..} = CMCComp {..}
  transformedSpineComp False meta compSort _ = SpineNest meta compSort []
  transformedSpineComp True meta compSort cols = SpineMerge meta compSort cols

colName :: Spine CMC -> PgColumnName
colName = \case
  SpinePrim _ meta -> meta.name
  SpineComp meta _ _ -> meta.name

indent ::
  Functor t =>
  t Text ->
  t Text
indent =
  fmap (" • " <>)

showType ::
  Spine CMC ->
  Text
showType =
  squote . \case
    SpinePrim _ (CMCPrim {colType = PgPrimName name}) -> name
    SpineComp (CMCComp {colType = PgTypeRef name}) _ _ -> name

-- TODO handle constraints (have to be added to CMC)
columnMismatch ::
  Maybe (Spine CMC) ->
  Maybe (Spine CMC) ->
  Text
columnMismatch Nothing (Just s) =
  [exon|A column '##{colName s}' with type #{showType s} was added.|]
columnMismatch (Just s) Nothing =
  [exon|The column '##{colName s}' with type #{showType s} was removed.|]
columnMismatch (Just golden) (Just current)
  | gname == cname =
    [exon|The type of the column '##{gname}' was changed from #{showType golden} to #{showType current}.|]
  | otherwise =
    [exon|The column '##{gname}' with type #{showType golden} was replaced with the column '##{cname}' with type #{showType current}.|]
  where
    gname = colName golden
    cname = colName current
columnMismatch Nothing Nothing =
  "Internal error"

compareType ::
  Text ->
  [Spine CMC] ->
  [Spine CMC] ->
  Maybe (NonEmpty Text)
compareType desc golden current =
  mismatches <$> nonEmpty (filter (uncurry (/=)) (zipWithLongest (,) golden current))
  where
    mismatches cols = [exon|#{desc} has mismatched columns:|] :| (indent (uncurry columnMismatch <$> toList cols))

compareComp ::
  MigrationVersion ->
  Maybe (TypeSpine CMC) ->
  Maybe (TypeSpine CMC) ->
  Maybe (NonEmpty Text)
compareComp (MigrationVersion version) =
  go
  where
    go Nothing Nothing =
      Nothing
    go Nothing (Just TypeSpine {name = PgTypeName name}) =
      Just [[exon|The type '#{name}' #{v} was added.|]]
    go (Just TypeSpine {name = PgTypeName name}) Nothing =
      Just [[exon|The type '#{name}' #{v} was removed.|]]
    go (Just golden) (Just current)
      | gname == cname =
        compareType [exon|The composite type '#{gname}' #{v}|] gcols ccols
      | otherwise =
        Just [[exon|The type '#{gname}' #{v} was replaced with a type named '#{cname}'.|]]
      where
        TypeSpine {name = PgTypeName gname, sub = gcols} = golden
        TypeSpine {name = PgTypeName cname, sub = ccols} = current
    v = [exon|[version #{show version}]|]

compareStep ::
  MigrationMetadata CMC ->
  MigrationMetadata CMC ->
  Maybe (NonEmpty Text)
compareStep golden current =
  join <$> nonEmpty (catMaybes mismatches)
  where
    mismatches =
      compareType [exon|The migration table '#{name}' [version #{showVersion golden}]|] golden.table current.table :
      zipWithLongest (compareComp golden.version) golden.types current.types
    PgTypeName name = golden.name

checkStep ::
  Maybe (MigrationMetadata CMC) ->
  Maybe (MigrationMetadata CMC) ->
  Maybe (NonEmpty Text)
checkStep Nothing Nothing =
  Nothing
checkStep Nothing (Just current) =
  let PgTypeName name = current.name
  in Just (pure [exon|A migration for '#{name}' [version #{showVersion current}] was added.|])
checkStep (Just golden) Nothing =
  let PgTypeName name = golden.name
  in Just (pure [exon|A migration for '#{name}' [version #{showVersion golden}] was removed.|])
checkStep (Just golden) (Just current) =
  compareStep golden current

checkMigrationConsistency ::
  TransformMeta tag CMC =>
  NonEmpty (MigrationMetadata CMC) ->
  NonEmpty (MigrationMetadata tag) ->
  Either (NonEmpty Text) ()
checkMigrationConsistency golden (fmap transform -> current) =
  maybeToLeft () (join <$> (nonEmpty (catMaybes (zipWithLongest checkStep (toList golden) (toList current)))))
