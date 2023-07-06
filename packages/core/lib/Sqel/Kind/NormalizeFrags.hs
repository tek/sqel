module Sqel.Kind.NormalizeFrags where

import Generics.SOP (NP (Nil, (:*)))

import Sqel.Data.Dd (type (:>) ((:>)))
import Sqel.Data.Fragment (Frag, Fragment)
import Sqel.Kind.List (type (++))
import Sqel.SOP.Error (Quoted)
import Sqel.SOP.NP (appendNP)

-- TODO more tuples
type NormalizedFrags :: Type -> [Frag ext]
type family NormalizedFrags expr where
  NormalizedFrags (l :> r) = NormalizedFrags l ++ NormalizedFrags r
  NormalizedFrags (l, r) = NormalizedFrags l ++ NormalizedFrags r
  NormalizedFrags (a1, a2, a3) = NormalizedFrags a1 ++ NormalizedFrags a2 ++ NormalizedFrags a3
  NormalizedFrags () = '[]
  NormalizedFrags (Fragment k) = '[k]

type InvalidFrags :: k
type family InvalidFrags where
  InvalidFrags =
    TypeError (
      "The argument of this clause has an invalid type." %
      "It should be one of the fields of the fragments bound by " <> Quoted "frags <- query" <> "," %
      "like " <> Quoted "frags.tables.users" <> " or " <> Quoted "frags.query" <> ", or combinations of fields like " %
      Quoted "(frags.users.name, frags.users.address)" <> "."
    )

type NormalizeFrags :: ∀ {ext} . Void -> Type -> [Frag ext] -> Constraint
class NormalizeFrags error expr frags | expr -> frags where
  normalizeFrags :: expr -> NP Fragment frags

instance (
    NormalizeFrags error l lfrags,
    NormalizeFrags error r rfrags,
    frags ~ lfrags ++ rfrags
  ) => NormalizeFrags error (l :> r) frags where
    normalizeFrags (l :> r) = appendNP @lfrags @rfrags (normalizeFrags @error l) (normalizeFrags @error r)

instance (
    NormalizeFrags error l lfrags,
    NormalizeFrags error r rfrags,
    frags ~ lfrags ++ rfrags
  ) => NormalizeFrags error (l, r) frags where
    normalizeFrags (l, r) = appendNP @lfrags @rfrags (normalizeFrags @error l) (normalizeFrags @error r)

instance (
    NormalizeFrags error a1 a1frags,
    NormalizeFrags error a2 a2frags,
    NormalizeFrags error a3 a3frags,
    frags ~ a1frags ++ a2frags ++ a3frags
  ) => NormalizeFrags error (a1, a2, a3) frags where
    normalizeFrags (a1, a2, a3) =
      appendNP (normalizeFrags @error a1) (appendNP @a2frags @a3frags (normalizeFrags @error a2) (normalizeFrags @error a3))

instance NormalizeFrags error (Fragment frag) '[frag] where
  normalizeFrags t = t :* Nil
