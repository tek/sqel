module Sqel.Class.DdField where

import Generics.SOP (NP)

import Sqel.Class.NamedFragment (MatchName, NoField)
import Sqel.Data.Dd (Dd)
import Sqel.Dd (DdNames)
import Sqel.SOP.HFind (HFind (HFindT, hfind))

type DdField :: ∀ {ext} . Symbol -> [Dd ext] -> Constraint
class DdField field ss where
  type DdFieldT field ss :: Dd ext
  ddField :: NP f ss -> f (DdFieldT field ss)

instance (
    err ~ NoField "field" field (DdNames ss) ss,
    pred ~ MatchName field,
    HFind err pred ss
  ) => DdField field ss where
    type DdFieldT field ss = HFindT (MatchName field) ss
    ddField = hfind @err @pred
