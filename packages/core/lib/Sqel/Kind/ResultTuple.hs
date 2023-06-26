module Sqel.Kind.ResultTuple where

import Sqel.Data.Dd (DdK)
import Sqel.Dd (DdTypes)

type ToTuple :: [Type] -> Type
type family ToTuple as where
  ToTuple '[] = ()
  ToTuple '[a] = a
  ToTuple [a0, a1] = (a0, a1)
  ToTuple [a0, a1, a2] = (a0, a1, a2)
  ToTuple [a0, a1, a2, a3] = (a0, a1, a2, a3)
  ToTuple [a0, a1, a2, a3, a4] = (a0, a1, a2, a3, a4)
  ToTuple [a0, a1, a2, a3, a4, a5] = (a0, a1, a2, a3, a4, a5)
  ToTuple [a0, a1, a2, a3, a4, a5, a6] = (a0, a1, a2, a3, a4, a5, a6)
  ToTuple [a0, a1, a2, a3, a4, a5, a6, a7] = (a0, a1, a2, a3, a4, a5, a6, a7)
  ToTuple [a0, a1, a2, a3, a4, a5, a6, a7, a8] = (a0, a1, a2, a3, a4, a5, a6, a7, a8)
  ToTuple [a0, a1, a2, a3, a4, a5, a6, a7, a8, a9] = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)
  ToTuple [a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10] = (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)

type ResultTuple :: [DdK ext] -> Type
type ResultTuple s = ToTuple (DdTypes s)
