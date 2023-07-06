module Sqel.Data.Clause where

import Generics.SOP (NP)

import Sqel.Data.ClauseConfig (ClauseFieldsFor)
import Sqel.Data.Codec (Decoder)
import Sqel.Kind.Error (PlainTypeError)
import Sqel.Kind.List (type (++))
import Sqel.Kind.Maybe (MaybeD (NothingD))
import Sqel.SOP.NP (appendNP)

type Clause :: Type -> Type -> Type
data Clause tag clause where
  Clause :: ClauseFieldsFor tag clause -> Clause tag clause

clauseFields :: Clause tag clause -> ClauseFieldsFor tag clause
clauseFields = \case
  Clause fields -> fields

type ClauseList tag cs = NP (Clause tag) cs

type Clauses :: Type -> [Type] -> Maybe [Type] -> Type -> Type
data Clauses tag cs result a where
  Clauses :: ClauseList tag cs -> MaybeD (NP Decoder) result -> a -> Clauses tag cs result a

instance Functor (Clauses tag result cs) where
  fmap f (Clauses cs res a) = Clauses cs res (f a)

unClauses :: Clauses tag cs result a -> ClauseList tag cs
unClauses = \case
  Clauses cs _ _ -> cs

pattern UnClauses :: ClauseList tag cs -> Clauses tag cs result a
pattern UnClauses cs <- (unClauses -> cs)
{-# complete UnClauses #-}

type AppendClauses :: Maybe [Type] -> Maybe [Type] -> Maybe [Type] -> Constraint
class AppendClauses l r result | l r -> result where
  appendResults :: MaybeD f l -> MaybeD f r -> MaybeD f result

instance AppendClauses 'Nothing 'Nothing 'Nothing where
  appendResults NothingD NothingD = NothingD

instance AppendClauses ('Just l) 'Nothing ('Just l) where
  appendResults res _ = res

instance AppendClauses 'Nothing ('Just r) ('Just r) where
  appendResults _ res = res

instance (
    PlainTypeError ("Cannot use two result producing clauses in a statement")
  ) => AppendClauses ('Just l) ('Just r) ('Just r) where
    appendResults = error "unreachable"

appendClauses ::
  AppendClauses resl resr result =>
  Clauses tag csl resl a ->
  Clauses tag csr resr b ->
  Clauses tag (csl ++ csr) result b
appendClauses (Clauses csl resl _) (Clauses csr resr b) =
  Clauses (appendNP csl csr) (appendResults resl resr) b

(+>) ::
  AppendClauses resl resr result =>
  Clauses tag csl resl a ->
  Clauses tag csr resr b ->
  Clauses tag (csl ++ csr) result b
(+>) = appendClauses

infixr 5 +>

type ClauseArgs :: Type -> Maybe Type -> Type
data ClauseArgs expr param where
  FragsOnly :: expr -> ClauseArgs expr 'Nothing
  FragsP :: expr -> param -> ClauseArgs expr ('Just param)

type ClauseParam :: Type -> Type -> Type
data ClauseParam field param =
  ClauseParam field param
