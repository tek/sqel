module Sqel.Class.DemoteConstraints where

import qualified Sqel.Data.Constraints
import Sqel.Data.Constraints (Constraints (Constraints), ConstraintsK (ConstraintsK))
import Sqel.Data.Sql (Sql (Sql))
import Sqel.SOP.Constraint (KnownSymbolsL, symbolTextsL)
import Sqel.SOP.HasGeneric (BoolVal (boolVal))

type DemoteConstraints :: ConstraintsK -> Constraint
class DemoteConstraints c where
  demoteConstraints :: Constraints

instance (
    BoolVal unique,
    BoolVal nullable,
    BoolVal primary,
    KnownSymbolsL custom
  ) => DemoteConstraints ('ConstraintsK unique nullable primary custom) where
    demoteConstraints =
      Constraints {
        unique = boolVal @unique,
        nullable = boolVal @nullable,
        primary = boolVal @primary,
        basic = Sql <$> toList (symbolTextsL @custom)
      }
