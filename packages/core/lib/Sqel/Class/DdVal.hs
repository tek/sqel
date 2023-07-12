module Sqel.Class.DdVal where

import Generics.SOP (AllZip, NP (Nil, (:*)), htrans, All, hcpure)

import Sqel.Data.Dd (Dd (Dd))
import Sqel.Dd (DdType, DdTypes)

type DdVal :: ∀ {ext} . (Type -> Type) -> Dd ext -> Type
newtype DdVal f s =
  DdVal { val :: f (DdType s) }

type UnDdVal :: ∀ {ext} . (Type -> Type) -> Dd ext -> Type -> Constraint
class UnDdVal f s a where
  unDdVal :: DdVal f s -> f a

instance a ~ DdType s => UnDdVal f s a where
  unDdVal (DdVal fa) = fa

type UnDdVals :: ∀ {ext} . [Dd ext] -> (Type -> Type) -> Constraint
class UnDdVals ss f where
  unDdVals :: NP (DdVal f) ss -> NP f (DdTypes ss)

instance (
    AllZip (UnDdVal f) ss (DdTypes ss)
  ) => UnDdVals ss f where
    unDdVals = htrans (Proxy @(UnDdVal f)) unDdVal

type DdPure :: ∀ {ext} . (Dd ext -> Constraint) -> [Dd ext] -> (Type -> Type) -> [Type] -> Constraint
class DdPure c ss f as | ss -> as where
  ddPure :: (∀ s . c s => DdVal f s) -> NP f as

instance (
    All c ss,
    as ~ DdTypes ss,
    UnDdVals ss f
  ) => DdPure c ss f as where
    ddPure cons =
      unDdVals @ss (hcpure (Proxy @c) cons)

-- | Not using @hmap@ and @DdTypes@ here because it results in insane compile performance.
type DdMap :: ∀ {ext} . (Dd ext -> Type) -> (Type -> Type) -> [Dd ext] -> [Type] -> Constraint
class DdMap f g ss as | ss -> as where
  ddmap :: (∀ s . f s -> g (DdType s)) -> NP f ss -> NP g as

instance DdMap f g '[] '[] where
  ddmap _ Nil = Nil

instance (
    DdMap f g ss as
  ) => DdMap f g ('Dd ext a s : ss) (a : as) where
    ddmap f (s :* ss) = f s :* ddmap f ss
