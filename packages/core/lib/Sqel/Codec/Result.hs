module Sqel.Codec.Result where

import Generics.SOP (NP (Nil, (:*)), SListI, hmap)

import Sqel.Class.DdVal (DdVal (DdVal), UnDdVals (unDdVals))
import Sqel.Codec.Product (ProdDecoder (prodDecoder))
import Sqel.Data.Clause (Clause (ResultClause, ResultsClause), ClauseK (ClauseK, ResultK, ResultsK), ClauseList)
import qualified Sqel.Data.Codec
import Sqel.Data.Codec (Decoder)
import Sqel.Data.Dd (DdK)
import Sqel.Data.Sqel (sqelCodec)
import Sqel.Dd (DdType, DdTypes)

type ClausesResult :: [ClauseK ext] -> [DdK ext]
type family ClausesResult cs where
  ClausesResult '[] = '[]
  ClausesResult ('ResultK _ s : _) = '[s]
  ClausesResult ('ResultsK _ s : _) = s
  ClausesResult (_ : ss) = ClausesResult ss

type FindResultDecoders :: ∀ {ext} . Type -> [DdK ext] -> [ClauseK ext] -> Constraint
class FindResultDecoders tag results cs | cs -> results where
  findResultDecoders :: ClauseList tag cs -> NP (DdVal Decoder) results

instance FindResultDecoders tag '[] '[] where
    findResultDecoders Nil = Nil

instance (
    FindResultDecoders tag results cs
  ) => FindResultDecoders tag results ('ClauseK frags : cs) where
    findResultDecoders (_ :* cs) = findResultDecoders cs

instance FindResultDecoders tag '[result] ('ResultK frags result : cs) where
    findResultDecoders (ResultClause _ s :* _) = DdVal (sqelCodec s).decoder :* Nil

instance (
    SListI results
  ) => FindResultDecoders tag results ('ResultsK frags results : cs) where
    findResultDecoders (ResultsClause _ ss :* _) = hmap (\ s -> DdVal (sqelCodec s).decoder) ss

type ResultDecoder :: ∀ {ext} . Type -> [DdK ext] -> Constraint
class ResultDecoder result sresults where
  resultDecoder :: NP (DdVal Decoder) sresults -> Decoder result

instance ResultDecoder () '[] where
  resultDecoder Nil = unit

instance DdType s ~ result => ResultDecoder result '[s] where
  resultDecoder (DdVal decoder :* Nil) = decoder

instance (
    sresults ~ s0 : s1 : ss,
    results ~ DdTypes sresults,
    UnDdVals sresults Decoder,
    ProdDecoder Decoder result results
  ) => ResultDecoder result (s0 : s1 : ss) where
    resultDecoder decoders =
      prodDecoder @Decoder @result @results (unDdVals @sresults decoders)

type ClausesResultDecoder :: ∀ {ext} . Type -> Type -> [ClauseK ext] -> Constraint
class ClausesResultDecoder tag result cs where
  clausesResultDecoder :: ClauseList tag cs -> Decoder result

instance (
    FindResultDecoders tag sresults cs,
    ResultDecoder result sresults
  ) => ClausesResultDecoder tag result cs where
    clausesResultDecoder clauses =
      resultDecoder (findResultDecoders @tag @sresults clauses)
