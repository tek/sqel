module Sqel.Class.ClauseResult where

import Generics.SOP (NP (Nil, (:*)))

import qualified Sqel.Data.Clause as Data
import Sqel.Data.Clause (ClauseK (ClauseK, ResultK, ResultsK), pattern ClauseResult, pattern NoClauseResult)
import Sqel.Data.Dd (DdK)
import Sqel.Data.Fragment (Frag (Frag), Frag0 (Frag0), Fragment (Fragment))
import Sqel.Data.Sqel (SqelFor)
import Sqel.Kind.ResultDds (ResultDds)

type ClauseResults :: ∀ {ext} . Type -> [Frag ext] -> [DdK ext] -> Constraint
class ClauseResults tag frags result | frags -> result where
  clauseResults :: NP Fragment frags -> NP (SqelFor tag) result

instance ClauseResults tag '[] '[] where
  clauseResults Nil = Nil

instance (
    ClauseResults tag frags result
  ) => ClauseResults tag ('Frag ('Frag0 tag sort s root comp) : frags) (s : result) where
    clauseResults (Fragment s :* frags) = s :* clauseResults frags

------------------------------------------------------------------------------------------------------------------------

type ClauseResult :: ∀ {ext} . Type -> [Frag ext] -> Maybe [DdK ext] -> Constraint
class ClauseResult tag frags result where
  clauseResult :: NP Fragment frags -> Data.ClauseResult tag result

instance ClauseResult tag frags 'Nothing where
  clauseResult _ = NoClauseResult

instance (
    ClauseResults tag frags result
  ) => ClauseResult tag frags ('Just result) where
    clauseResult frags = ClauseResult (clauseResults frags)

------------------------------------------------------------------------------------------------------------------------

type FromResultDds :: Type -> Maybe [DdK ext] -> ClauseK ext
type family FromResultDds clause result = r | r -> clause where
  FromResultDds clause 'Nothing = 'ClauseK clause
  FromResultDds clause ('Just '[]) = 'ClauseK clause
  FromResultDds clause ('Just '[result]) = 'ResultK clause result
  FromResultDds clause ('Just results) = 'ResultsK clause results

type ClauseResultDds :: ∀ ext . Type -> [Frag ext] -> Maybe [DdK ext]
type family ClauseResultDds clause frags where
  ClauseResultDds clause frags = ResultDds clause frags
