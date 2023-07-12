module Sqel.Class.TableTypes where

import Generics.SOP (NP (Nil, (:*)))

import Sqel.Data.Dd (Dd (Dd), Inc (Merge, Nest), Struct (Comp, Prim))
import Sqel.Data.Sqel (SqelFor, pattern SqelMerge, pattern SqelNest)
import Sqel.Dd (DdStruct, DdSub)
import Sqel.Kind.List (type (++))
import Sqel.SOP.NP (appendNP)

type SubK :: ∀ {ext} . [Dd ext] -> [Dd ext]
type family SubK s where
  SubK '[] = '[]
  SubK (s : ss) = StructK s (DdStruct s) ++ SubK ss

type StructK :: Dd ext -> Struct ext -> [Dd ext]
type family StructK s s' where
  StructK _ ('Prim _) = '[]
  StructK _ ('Comp _ _ 'Merge sub) = SubK sub
  StructK s ('Comp _ _ 'Nest sub) = s : SubK sub

type TableTypesK :: Dd ext -> [Dd ext]
type family TableTypesK s where
  TableTypesK s = SubK (DdSub s)

type SubTypes :: ∀ {ext} . [Dd ext] -> Constraint
class SubTypes s where
  subTypes :: NP (SqelFor tag) s -> NP (SqelFor tag) (SubK s)

instance SubTypes '[] where
  subTypes Nil = Nil

instance (
    StructTypes s,
    SubTypes ss
  ) => SubTypes (s : ss) where
    subTypes (s :* ss) =
      appendNP (structTypes s) (subTypes ss)

type StructTypes :: Dd ext -> Constraint
class StructTypes s where
  structTypes :: SqelFor tag s -> NP (SqelFor tag) (StructK s (DdStruct s))

instance StructTypes ('Dd ext a ('Prim prim)) where
  structTypes _ = Nil

instance (
    SubTypes sub
  ) => StructTypes ('Dd ext a ('Comp tsel sort 'Nest sub)) where
    structTypes s@(SqelNest _ _ sub _) = s :* subTypes sub

instance (
    SubTypes sub
  ) => StructTypes ('Dd ext a ('Comp tsel sort 'Merge sub)) where
    structTypes (SqelMerge _ _ sub _) = subTypes sub

type SqelTableTypes :: ∀ {ext} . Dd ext -> Constraint
class SqelTableTypes s where
  sqelTableTypes :: SqelFor tag s -> NP (SqelFor tag) (TableTypesK s)

instance SqelTableTypes ('Dd ext a ('Prim prim)) where
  sqelTableTypes _ = Nil

instance (
    SubTypes sub
  ) => SqelTableTypes ('Dd ext a ('Comp tsel sort inc sub)) where
    sqelTableTypes = \case
      SqelNest _ _ sub _ -> subTypes sub
      SqelMerge _ _ sub _ -> subTypes sub
