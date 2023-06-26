module Sqel.Constraints where

import qualified Sqel.Data.Constraints
import Sqel.Data.Constraints (Constraints, ConstraintsK (ConstraintsK), DefaultConstraints)
import Sqel.Data.Mods (ColumnConstraint)
import Sqel.Data.Mods.Nullable (Nullable)
import Sqel.Data.Mods.PrimaryKey (PrimaryKey)
import Sqel.Data.Mods.Unique (Unique)

type AddConstraint :: ConstraintsK -> Type -> ConstraintsK
type family AddConstraint acc mod where
  AddConstraint ('ConstraintsK u _ p c) (Nullable _) = 'ConstraintsK u 'True p c
  AddConstraint ('ConstraintsK _ n _ c) PrimaryKey = 'ConstraintsK 'True n 'True c
  AddConstraint ('ConstraintsK _ n p c) Unique = 'ConstraintsK 'True n p c
  AddConstraint ('ConstraintsK u n p c) (ColumnConstraint constr) = 'ConstraintsK u n p (constr : c)
  AddConstraint acc _ = acc

type ColumnConstraintsK :: ConstraintsK -> [Type] -> ConstraintsK
type family ColumnConstraintsK acc mods where
  ColumnConstraintsK acc '[] = acc
  ColumnConstraintsK acc (mod : mods) = ColumnConstraintsK (AddConstraint acc mod) mods

type ConstraintsFor :: [Type] -> ConstraintsK
type family ConstraintsFor mods where
  ConstraintsFor mods = ColumnConstraintsK DefaultConstraints mods

constrUnique :: Constraints -> Bool
constrUnique c = c.unique || c.primary
