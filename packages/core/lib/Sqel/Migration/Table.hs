module Sqel.Migration.Table where

import qualified Data.Map as Map

import Sqel.Class.TableTypes (SqelTableTypes (sqelTableTypes), TableTypesK)
import Sqel.Data.Dd (DdK)
import Sqel.Data.Migration (CompAction)
import Sqel.Data.PgTypeName (PgCompName)
import Sqel.Data.Sqel (SqelFor)
import Sqel.Migration.Type (TypeChanges (typeChanges))

type TableTypeChanges :: ∀ {ext} . Type -> DdK ext -> DdK ext -> Constraint
class TableTypeChanges tag old new where
  tableTypeChanges :: SqelFor tag old -> SqelFor tag new -> Map PgCompName (CompAction tag)

instance (
    SqelTableTypes old,
    SqelTableTypes new,
    oldTypes ~ TableTypesK old,
    newTypes ~ TableTypesK new,
    TypeChanges tag oldTypes newTypes
  ) => TableTypeChanges tag old new where
    tableTypeChanges old new =
      Map.fromList (typeChanges oldTypes newTypes)
      where
        oldTypes = sqelTableTypes old
        newTypes = sqelTableTypes new
