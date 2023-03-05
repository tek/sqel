module Sqel.Migration.Data.TypeStatus where

import Prettyprinter (Pretty (pretty), concatWith, parens, squotes, surround, (<+>))

import Sqel.Data.PgType (PgColumnName, PgPrimName, PgTypeRef)

data MismatchReason =
  ExtraneousColumn
  |
  MissingColumn
  |
  TypeMismatch
  deriving stock (Eq, Show, Generic)

instance Pretty MismatchReason where
  pretty = \case
    ExtraneousColumn -> "extraneous"
    MissingColumn -> "missing"
    TypeMismatch -> "type mismatch"

data ColumnMismatch =
  ColumnMismatch {
    name :: PgColumnName,
    tpe :: Either PgTypeRef PgPrimName,
    reason :: MismatchReason
  }
  deriving stock (Eq, Show, Generic)

instance Pretty ColumnMismatch where
  pretty ColumnMismatch {..} =
    squotes (pretty name )<+> parens (pretty reason <+> squotes (either pretty pretty tpe))

data TypeStatus =
  Absent
  |
  Match
  |
  Mismatch [ColumnMismatch]
  deriving stock (Eq, Show, Generic)

instance Pretty TypeStatus where
  pretty = \case
    Absent -> "absent"
    Match -> "match"
    Mismatch cols -> "mismatches:" <+> concatWith (surround " | ") (pretty <$> cols)
