module Sqel.Class.DdVal where

import Generics.SOP (All, AllZip, NP, SListI, hcpure, hmap, htrans)

import Sqel.Data.Dd (DdK)
import Sqel.Dd (DdType, DdTypes)

type DdVal :: ∀ {ext} . (Type -> Type) -> DdK ext -> Type
newtype DdVal f s =
  DdVal { val :: f (DdType s) }

type UnDdVal :: ∀ {ext} . (Type -> Type) -> DdK ext -> Type -> Constraint
class UnDdVal f s a where
  unDdVal :: DdVal f s -> f a

instance a ~ DdType s => UnDdVal f s a where
  unDdVal (DdVal fa) = fa

type UnDdVals :: ∀ {ext} . [DdK ext] -> (Type -> Type) -> Constraint
class UnDdVals ss f where
  unDdVals :: NP (DdVal f) ss -> NP f (DdTypes ss)

instance (
    AllZip (UnDdVal f) ss (DdTypes ss)
  ) => UnDdVals ss f where
    unDdVals = htrans (Proxy @(UnDdVal f)) unDdVal

type DdPure :: ∀ {ext} . (DdK ext -> Constraint) -> [DdK ext] -> (Type -> Type) -> [Type] -> Constraint
class DdPure c ss f as | ss -> as where
  ddPure :: (∀ s . c s => DdVal f s) -> NP f as

instance (
    All c ss,
    as ~ DdTypes ss,
    UnDdVals ss f
  ) => DdPure c ss f as where
    ddPure cons =
      unDdVals @ss (hcpure (Proxy @c) cons)

class DdMap f g ss where
  ddmap :: (∀ s . f s -> g (DdType s)) -> NP f ss -> NP g (DdTypes ss)

instance (
    SListI ss,
    UnDdVals ss g
  ) => DdMap f g ss where
    ddmap f ss = unDdVals @ss (hmap (DdVal . f) ss)
