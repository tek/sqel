module Sqel.Data.PgTypeName where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=))
import Data.Dependent.Sum.Orphans ()
import Data.GADT.Compare.TH (deriveGEq)
import Data.GADT.Show (GShow (gshowsPrec))
import Data.Some (Some (Some), foldSome)
import Exon (exon)
import Prettyprinter (Pretty (pretty))

import Sqel.Data.Name (NamePrefix (DefaultPrefix))
import Sqel.Data.Sel (TSel (TSel), TypeName)
import Sqel.Data.Sql (ToSql (toSql), sqlQuote)
import Sqel.SOP.Constraint (symbolText)
import Sqel.Text.DbIdentifier (dbIdentifierT)

-- TODO this should be named CompName for consistency, with @Type@ denoting a type as opposed to a table
type PgTypeName :: Bool -> Type
data PgTypeName table where
  UnsafePgTableName :: Text -> PgTypeName 'True
  UnsafePgCompName :: Text -> PgTypeName 'False

type PgTableName =
  PgTypeName 'True

type PgCompName =
  PgTypeName 'False

getPgTypeName :: PgTypeName table -> Text
getPgTypeName = \case
  UnsafePgTableName n -> n
  UnsafePgCompName n -> n

getSomePgTypeName :: Some PgTypeName -> Text
getSomePgTypeName = foldSome getPgTypeName

pattern PgTypeName :: Text -> PgTypeName table
pattern PgTypeName name <- (getPgTypeName -> name)
{-# complete PgTypeName #-}

pattern PgTableName :: () => table ~ 'True => Text -> PgTypeName table
pattern PgTableName name <- (UnsafePgTableName name)

pattern PgCompName :: () => table ~ 'False => Text -> PgTypeName table
pattern PgCompName name <- (UnsafePgCompName name)

{-# complete PgTableName, PgCompName #-}

pattern PgOnlyTableName :: Text -> PgTypeName 'True
pattern PgOnlyTableName name <- (UnsafePgTableName name)

{-# complete PgOnlyTableName #-}

pattern PgOnlyCompName :: Text -> PgTypeName 'False
pattern PgOnlyCompName name <- (UnsafePgCompName name)

{-# complete PgOnlyCompName #-}

instance Eq (PgTypeName table) where
  UnsafePgTableName l == UnsafePgTableName r = l == r
  UnsafePgCompName l == UnsafePgCompName r = l == r

instance Show (PgTypeName table) where
  showsPrec d =
    showParen (d > 10) . \case
      UnsafePgTableName n -> [exon|PgTableName #{showsPrec 11 n}|]
      UnsafePgCompName n -> [exon|PgCompName #{showsPrec 11 n}|]

deriveGEq ''PgTypeName

instance GShow PgTypeName where gshowsPrec = showsPrec

instance Pretty (PgTypeName table) where
  pretty (UnsafePgCompName n) = pretty n
  pretty (UnsafePgTableName n) = pretty n

instance ToSql (PgTypeName table) where
  toSql (PgTypeName n) =
    sqlQuote n

instance ToSql (Some (PgTypeName)) where
  toSql (Some (PgTypeName n)) =
    sqlQuote n

instance FromJSON PgTableName where
  parseJSON v = parseJSON v >>= foldSome \case
    UnsafePgTableName n -> pure (UnsafePgTableName n)
    UnsafePgCompName n -> fail [exon|Got PgCompName for PgTableName: #{toString n}|]

instance FromJSON PgCompName where
  parseJSON v = parseJSON v >>= foldSome \case
    UnsafePgCompName n -> pure (UnsafePgCompName n)
    UnsafePgTableName n -> fail [exon|Got PgTableName for PgCompName: #{toString n}|]

instance FromJSON (Some PgTypeName) where
  parseJSON =
    withObject "PgTypeName" \ o -> do
      tag :: Text <- o .: "tag"
      n <- o .: "value"
      case tag of
        "table" -> pure (Some (UnsafePgTableName n))
        "comp" -> pure (Some (UnsafePgCompName n))
        other -> fail [exon|Invalid tag '#{toString other}' for PgTypeName|]

instance ToJSON (PgTypeName t) where
  toJSON n =
    object [
      "tag" .= tag,
      "value" .= getPgTypeName n
    ]
    where
      tag :: Text
      tag = case n of
        UnsafePgTableName _ -> "table"
        UnsafePgCompName _ -> "comp"

pgTableName ::
  Text ->
  PgTypeName 'True
pgTableName =
  UnsafePgTableName . dbIdentifierT

pgCompName ::
  Text ->
  PgTypeName 'False
pgCompName name =
  UnsafePgCompName (dbIdentifierT name)

instance IsString PgTableName where
  fromString =
    pgTableName . fromString

instance IsString PgCompName where
  fromString =
    pgCompName . fromString

instance Ord (PgTypeName table) where
  compare = comparing getPgTypeName

type MkPgTypeName :: NamePrefix -> Symbol -> Bool -> Symbol -> Constraint
class KnownSymbol tname => MkPgTypeName prefix name table tname | prefix name table -> tname where
  pgTypeName :: PgTypeName table

instance (
    KnownSymbol name
  ) => MkPgTypeName 'DefaultPrefix name 'True name where
    pgTypeName = pgTableName (symbolText @name)

instance (
    TypeName prefix name tname
  ) => MkPgTypeName prefix name 'False tname where
    pgTypeName = pgCompName (symbolText @tname)

class TselPgTypeName tsel table tname | tsel table -> tname where
  tselPgTypeName :: PgTypeName table

instance (
    MkPgTypeName prefix name table tname
  ) => TselPgTypeName ('TSel prefix name) table tname where
    tselPgTypeName = pgTypeName @prefix @name

type PgTypeNameSym :: Bool -> Symbol -> Constraint
class PgTypeNameSym table name where
  pgTypeNameSym :: PgTypeName table

instance (
    KnownSymbol name
  ) => PgTypeNameSym 'True name where
  pgTypeNameSym =
    pgTableName (symbolText @name)

instance (
    KnownSymbol name
  ) => PgTypeNameSym 'False name where
  pgTypeNameSym =
    pgCompName (symbolText @name)
