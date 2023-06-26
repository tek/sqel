module Sqel.Class.Mods where

import Fcf (ConstFn, Eval, Exp, TyEq, type (@@))
import Fcf.Class.Foldable (Any)

import Sqel.Data.Mods.PrimaryKey (PrimaryKey)
import Sqel.Data.Mods.Unique (Unique)

type FindMod :: (k -> Type) -> [Type] -> Maybe k
type family FindMod f mods where
  FindMod _ '[] = 'Nothing
  FindMod f (f a : _) = 'Just a
  FindMod f (_ : mods) = FindMod f mods

type ModPred = [Type] -> Exp Bool

data PredHasMod :: Type -> [Type] -> Exp Bool
type instance Eval (PredHasMod target mods) =
  Any (TyEq target) @@ mods

data MatchAnyMod :: [Type] -> Type -> Exp Bool
type instance Eval (MatchAnyMod target mod) =
  Any (TyEq mod) @@ target

data PredAnyMod :: [Type] -> [Type] -> Exp Bool
type instance Eval (PredAnyMod target mods) =
  Any (MatchAnyMod target) @@ mods

type IsUnique = PredAnyMod [Unique, PrimaryKey]

type PredTrue = ConstFn 'True

type HasAnyMod :: [Type] -> [Type] -> Bool
type HasAnyMod target mods = Eval (PredAnyMod target mods)

type HasMod :: Type -> [Type] -> Bool
type HasMod target mods = Eval (PredHasMod target mods)
