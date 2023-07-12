module Sqel.Migration.Data.TransformStep where

import Sqel.Data.Dd (Dd)
import Sqel.Data.Sqel (SqelFor)
import Sqel.Dd (DdType)

type TransformStep :: ∀ {ext} . Type -> (Type -> Type) -> Type -> Dd ext -> Type
data TransformStep tag m old new =
  TransformStep {
    new :: SqelFor tag new,
    trans :: [old] -> m [DdType new]
  }
