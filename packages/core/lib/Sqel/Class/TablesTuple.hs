module Sqel.Class.TablesTuple where

import Generics.SOP (NP (Nil, (:*)))

import Sqel.Data.Dd (Dd)
import Sqel.Data.Sqel (SqelFor)

type TablesTuple :: Type -> Type -> [Dd ext] -> Constraint
class TablesTuple tag expr tables | expr -> tables tag where
  tablesTuple :: expr -> NP (SqelFor tag) tables

instance TablesTuple tag (NP (SqelFor tag) tables) tables where
  tablesTuple = id

instance TablesTuple tag (SqelFor tag table) '[table] where
  tablesTuple t = t :* Nil

instance TablesTuple tag (SqelFor tag t1, SqelFor tag t2) [t1, t2] where
  tablesTuple (t1, t2) = t1 :* t2 :* Nil

instance TablesTuple tag (SqelFor tag t1, SqelFor tag t2, SqelFor tag t3) [t1, t2, t3] where
  tablesTuple (t1, t2, t3) = t1 :* t2 :* t3 :* Nil

instance TablesTuple tag (SqelFor tag t1, SqelFor tag t2, SqelFor tag t3, SqelFor tag t4) [t1, t2, t3, t4] where
  tablesTuple (t1, t2, t3, t4) = t1 :* t2 :* t3 :* t4 :* Nil
