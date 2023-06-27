module Sqel.Dsl.Mod where

import Fcf (Eval, Exp)
import Prelude hiding (Mod)

import Sqel.Data.Dd (Dd0, DdK (Dd), Ext0 (Ext0))
import Sqel.Kind.List (type (++))

type Mod :: Type -> Type -> Type
data Mod mod spec

type Mods :: [Type] -> Type -> Type
data Mods mods spec

type ModWith :: (Dd0 -> Exp Dd0) -> Type -> Type -> Type
data ModWith f mod spec

type ModTrans :: (Dd0 -> Exp Dd0) -> Type -> Type
data ModTrans f spec

------------------------------------------------------------------------------------------------------------------------

type AddMods :: [Type] -> Dd0 -> Dd0
type family AddMods mods s where
  AddMods new ('Dd ('Ext0 sel old) a s) = 'Dd ('Ext0 sel (new ++ old)) a s

type AddMod :: Type -> Dd0 -> Dd0
type family AddMod mod s where
  AddMod mod ('Dd ('Ext0 sel old) a s) = 'Dd ('Ext0 sel (mod : old)) a s

type AddModWith :: (Dd0 -> Exp Dd0) -> Type -> Dd0 -> Dd0
type family AddModWith f mod s where
  AddModWith f mod s = AddMod mod (Eval (f s))

type ApplyMod :: (Type -> Type) -> Dd0 -> Dd0
type family ApplyMod mod s where
  ApplyMod (Mod mod) s = AddMod mod s
  ApplyMod (ModTrans f) s = Eval (f s)
  ApplyMod (ModWith f mod) s = AddModWith f mod s
