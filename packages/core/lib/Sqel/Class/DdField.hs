module Sqel.Class.DdField where

import Fcf (Eval, Exp, TyEq)
import Generics.SOP (NP)

import Sqel.Data.Dd (Dd, DdK)
import Sqel.Dd (DdKName, DdKNames, DdNames)
import Sqel.SOP.Error (BulletedLines, Quoted)
import Sqel.SOP.HFind (HFind (HFindT, hfind))

type NoField :: Symbol -> [Symbol] -> k
type family NoField field avail where
  NoField field avail =
    TypeError ("No field named " <> Quoted field <> ". Available fields:" % BulletedLines avail)

data MatchName :: Symbol -> DdK ext -> Exp Bool
type instance Eval (MatchName field s) = Eval (TyEq field (DdKName s))

type DdKFieldT :: Symbol -> [DdK ext] -> DdK ext
type family DdKFieldT field ss where
  DdKFieldT field ss =
    HFindT (MatchName field) ss

type DdKField :: ∀ {ext} . Symbol -> [DdK ext] -> Constraint
class DdKField field ss where
  ddKField :: NP f ss -> f (DdKFieldT field ss)

instance (
    err ~ NoField field (DdKNames ss),
    pred ~ MatchName field,
    HFind err pred ss
  ) => DdKField field ss where
    ddKField = hfind @err @pred

type DdField :: Symbol -> [Dd] -> Constraint
class DdField field ss where
  type DdFieldT field ss :: Dd
  ddField :: NP f ss -> f (DdFieldT field ss)

instance (
    err ~ NoField field (DdNames ss),
    pred ~ MatchName field,
    HFind err pred ss
  ) => DdField field ss where
    type DdFieldT field ss = HFindT (MatchName field) ss
    ddField = hfind @err @pred
