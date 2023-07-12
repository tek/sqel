module Sqel.Dsl.Prim where

type PrimAuto :: Type
data PrimAuto

type PrimAs :: Symbol -> Type
data PrimAs name

type PrimUsing :: Type -> Type
data PrimUsing prim

type PrimWith :: Symbol -> Type -> Type
data PrimWith name a

type Prim :: ∀ k . k
type family Prim where
  Prim @Type = PrimAuto
  Prim @(Type -> Type) = PrimUsing
  Prim @(Symbol -> Type) = PrimAs
  Prim @(Symbol -> Type -> Type) = PrimWith

type AllAuto :: Type -> [k] -> [Type]
type family AllAuto spec as where
  AllAuto _ '[] = '[]
  AllAuto spec (_ : as) = spec : AllAuto spec as

type PrimEnum :: Type
data PrimEnum

type PrimJson :: Type
data PrimJson

type PrimJsonb :: Type
data PrimJsonb

type Param :: Type -> Type
data Param spec
