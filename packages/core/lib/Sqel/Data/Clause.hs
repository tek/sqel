module Sqel.Data.Clause where

import Generics.SOP (NP)

import Sqel.Data.ClauseConfig (ClauseFieldsFor)
import Sqel.Data.Dd (DdK)
import Sqel.Data.Sqel (SqelFor)
import Sqel.Kind.List (type (++))
import Sqel.SOP.NP (appendNP)

-- TODO the result can be a list of types, rather than the Dds – we already have the derived codecs
-- this would probably require/allow Sqel to drop the Dd param in favor of the result type at some point
type ClauseK :: Type -> Type
data ClauseK ext =
  ClauseK {
    clause :: Type
  }
  |
  ResultK {
    clause :: Type,
    result :: DdK ext
  }
  |
  ResultsK {
    clause :: Type,
    results :: [DdK ext]
  }

type ClauseTag :: ∀ {ext} . ClauseK ext -> Type
type family ClauseTag clause where
  ClauseTag ('ClauseK clause) = clause
  ClauseTag ('ResultK clause _) = clause
  ClauseTag ('ResultsK clause _) = clause

type ClauseResult :: ∀ {ext} . Type -> Maybe [DdK ext] -> Type
data ClauseResult tag result where
  ClauseResult :: NP (SqelFor tag) result -> ClauseResult tag ('Just result)
  NoClauseResult :: ClauseResult tag 'Nothing

type Clause :: ∀ {ext} . Type -> ClauseK ext -> Type
data Clause tag k where
  Clause :: ClauseFieldsFor tag clause -> Clause tag ('ClauseK clause)
  ResultClause :: ClauseFieldsFor tag clause -> SqelFor tag result -> Clause tag ('ResultK clause result)
  ResultsClause :: ClauseFieldsFor tag clause -> NP (SqelFor tag) results -> Clause tag ('ResultsK clause results)

clauseFields :: Clause tag k -> ClauseFieldsFor tag (ClauseTag k)
clauseFields = \case
  Clause fields -> fields
  ResultClause fields _ -> fields
  ResultsClause fields _ -> fields

type ClauseList tag cs = NP (Clause tag) cs

type Clauses :: Type -> [ClauseK ext] -> Type -> Type
data Clauses tag cs a =
  Clauses { clauses :: ClauseList tag cs, value :: a }

instance Functor (Clauses tag cs) where
  fmap f (Clauses cs a) = Clauses cs (f a)

unClauses :: Clauses tag cs a -> ClauseList tag cs
unClauses (Clauses cs _) = cs

appendClauses :: Clauses tag csl a -> Clauses tag csr b -> Clauses tag (csl ++ csr) b
appendClauses (Clauses csl _) (Clauses csr b) =
  Clauses (appendNP csl csr) b

(+>) :: Clauses tag csl a -> Clauses tag csr b -> Clauses tag (csl ++ csr) b
(+>) = appendClauses

infixr 5 +>

type ClauseArgs :: Type -> Maybe Type -> Type
data ClauseArgs expr param where
  FragsOnly :: expr -> ClauseArgs expr 'Nothing
  FragsP :: expr -> param -> ClauseArgs expr ('Just param)

type ClauseParam :: Type -> Type -> Type
data ClauseParam field param =
  ClauseParam field param
