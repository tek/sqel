module Sqel.Migration.Data.TransformStep where

import Sqel.Data.Dd (DdK)
import Sqel.Data.Sqel (SqelFor)
import Sqel.Dd (DdType)

type TransformStep :: ∀ {ext} . Type -> (Type -> Type) -> Type -> DdK ext -> Type
data TransformStep tag m old new =
  TransformStep {
    new :: SqelFor tag new,
    trans :: [old] -> m [DdType new]
  }
