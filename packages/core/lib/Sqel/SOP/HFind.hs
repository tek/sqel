module Sqel.SOP.HFind where

import Fcf (Eval, Exp)
import Generics.SOP (NP, hd, tl)

type HFindPred k = k -> Exp Bool

type Match :: ∀ {k} . Void -> Bool -> HFindPred k -> k -> [k] -> Constraint
class Match err match pred a0 as where
  type MatchT match pred a0 as :: k
  match :: NP f (a0 : as) -> f (MatchT match pred a0 as)

instance Match err 'True pred a0 as where
  type MatchT 'True pred a0 as = a0
  match = hd

instance (
    HFind err pred as
  ) => Match err 'False pred a0 as where
    type MatchT 'False pred a0 as = HFindT pred as
    match = hfind @err @pred . tl

type HFind :: ∀ {k} . Void -> HFindPred k -> [k] -> Constraint
class HFind err pred as where
  type HFindT pred as :: k
  hfind :: NP f as -> f (HFindT pred as)

instance (
    match ~ Eval (pred a0),
    Match err match pred a0 as
  ) => HFind err pred (a0 : as) where
    type HFindT pred (a0 : as) = MatchT (Eval (pred a0)) pred a0 as
    hfind = match @err @match @pred
