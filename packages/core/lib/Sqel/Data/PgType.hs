module Sqel.Data.PgType where

import Data.Aeson (FromJSON, ToJSON)
import Prettyprinter (Pretty (pretty))

import Sqel.Data.Sql (Sql (Sql), ToSql (toSql), sqlQuote)
import Sqel.SOP.Constraint (symbolText)
import Sqel.Text.DbIdentifier (dbIdentifierT, dbSymbol)
import Sqel.Text.Quote (dquote)

newtype PgPrimName =
  PgPrimName { unPgPrimName :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, Semigroup, Monoid, ToJSON, FromJSON)

instance Pretty PgPrimName where
  pretty (PgPrimName n) = pretty n

instance ToSql PgPrimName where
  toSql =
    Sql . (.unPgPrimName)

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
    sqlQuote . (.unPgColumnName)

pgColumnName ::
  Text ->
  PgColumnName
pgColumnName n =
  PgColumnName (dbIdentifierT n)

pgColumnNameSym ::
  ∀ name .
  KnownSymbol name =>
  PgColumnName
pgColumnNameSym =
  pgColumnName (symbolText @name)

instance IsString PgColumnName where
  fromString = pgColumnName . fromString

columnNameQuoted :: PgColumnName -> Sql
columnNameQuoted (PgColumnName name) = Sql (dquote name)

newtype PgTypeRef =
  PgTypeRef { unPgTypeRef :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, ToJSON, FromJSON)

instance Pretty PgTypeRef where
  pretty (PgTypeRef n) = pretty n

instance ToSql PgTypeRef where
  toSql = sqlQuote . (.unPgTypeRef)

pgTypeRef ::
  Text ->
  PgTypeRef
pgTypeRef n =
  PgTypeRef (dbIdentifierT n)

pgTypeRefSym ::
  ∀ tname .
  KnownSymbol tname =>
  PgTypeRef
pgTypeRefSym =
  pgTypeRef (symbolText @tname)
