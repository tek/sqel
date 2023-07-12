module Sqel.Class.DdField where

import Generics.SOP (NP)

import Sqel.Class.NamedFragment (MatchName, NoField)
import Sqel.Data.Dd (Dd)
import Sqel.Dd (DdKNames)
import Sqel.SOP.HFind (HFind (HFindT, hfind))

type DdKFieldT :: Symbol -> [Dd ext] -> Dd ext
type family DdKFieldT field ss where
  DdKFieldT field ss =
    HFindT (MatchName field) ss

type DdKField :: ∀ {ext} . Symbol -> [Dd ext] -> Constraint
class DdKField field ss where
  ddKField :: NP f ss -> f (DdKFieldT field ss)

instance (
    err ~ NoField "field" field (DdKNames ss) ss,
    pred ~ MatchName field,
    HFind err pred ss
  ) => DdKField field ss where
    ddKField = hfind @err @pred

type DdField :: Symbol -> [Dd ext] -> Constraint
class DdField field ss where
  type DdFieldT field ss :: Dd ext
  ddField :: NP f ss -> f (DdFieldT field ss)

instance (
    err ~ NoField "field" field (DdKNames ss) ss,
    pred ~ MatchName field,
    HFind err pred ss
  ) => DdField field ss where
    type DdFieldT field ss = HFindT (MatchName field) ss
    ddField = hfind @err @pred
