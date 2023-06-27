module Sqel.Dsl.Prim where

type PrimAuto :: Type
data PrimAuto

type PrimAs :: Symbol -> Type
data PrimAs name

type PrimWith :: Symbol -> Type -> Type
data PrimWith name a

type Prim :: ∀ k . k
type family Prim where
  Prim @Type = PrimAuto
  Prim @(Symbol -> Type) = PrimAs
  Prim @(Symbol -> Type -> Type) = PrimWith

type AllAuto :: [k] -> [Type]
type family AllAuto as where
  AllAuto '[] = '[]
  AllAuto (_ : as) = PrimAuto : AllAuto as

type Param :: Type -> Type
data Param spec
