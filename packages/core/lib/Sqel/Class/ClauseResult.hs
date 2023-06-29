module Sqel.Class.ClauseResult where

import Generics.SOP (NP (Nil, (:*)))

import qualified Sqel.Data.Codec as Codec
import Sqel.Data.Codec (Decoder)
import Sqel.Data.Fragment (Frag (Frag), Frag0 (Frag0), Fragment (Fragment))
import Sqel.Data.Sqel (sqelCodec)
import Sqel.Dd (DdType)
import Sqel.Kind.Maybe (MaybeD (JustD, NothingD))

type ClauseDecoders = MaybeD (NP Decoder)

type ClauseResults :: ∀ {ext} . [Frag ext] -> [Type] -> Constraint
class ClauseResults frags result | frags -> result where
  clauseResults :: NP Fragment frags -> NP Decoder result

instance ClauseResults '[] '[] where
  clauseResults Nil = Nil

instance (
    a ~ DdType s,
    ClauseResults frags result
  ) => ClauseResults ('Frag ('Frag0 tag sort s root comp) : frags) (a : result) where
    clauseResults (Fragment s :* frags) = (sqelCodec s).decoder :* clauseResults frags

------------------------------------------------------------------------------------------------------------------------

type ClauseResult :: ∀ {ext} . Bool -> [Frag ext] -> Maybe [Type] -> Constraint
class ClauseResult isResult frags result | isResult frags -> result where
  clauseResult :: NP Fragment frags -> ClauseDecoders result

instance ClauseResult 'False frags 'Nothing where
  clauseResult _ = NothingD

instance (
    ClauseResults frags result
  ) => ClauseResult 'True frags ('Just result) where
    clauseResult frags = JustD (clauseResults frags)
