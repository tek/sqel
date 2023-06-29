module Sqel.Codec.Result where

import Generics.SOP (NP (Nil, (:*)))

import Sqel.Codec.Product (ProdDecoder (prodDecoder))
import Sqel.Data.Codec (Decoder)
import Sqel.Kind.Maybe (MaybeD (JustD, NothingD))

type ResultDecoder :: [Type] -> Type -> Constraint
class ResultDecoder results result where
  resultDecoder :: NP Decoder results -> Decoder result

instance ResultDecoder '[] () where
  resultDecoder Nil = unit

instance a ~ result => ResultDecoder '[a] result where
  resultDecoder (decoder :* Nil) = decoder

instance (
    results ~ a0 : a1 : as,
    ProdDecoder Decoder result results
  ) => ResultDecoder (a0 : a1 : as) result where
    resultDecoder decoders =
      prodDecoder @Decoder @result @results decoders

-- TODO rename, maybe Results
type ClausesResultDecoder :: Maybe [Type] -> Type -> Constraint
class ClausesResultDecoder results result where
  clausesResultDecoder :: MaybeD (NP Decoder) results -> Decoder result

instance (
    ResultDecoder results result
  ) => ClausesResultDecoder ('Just results) result where
    clausesResultDecoder (JustD results) =
      resultDecoder results

instance ClausesResultDecoder 'Nothing () where
  clausesResultDecoder NothingD = unit
