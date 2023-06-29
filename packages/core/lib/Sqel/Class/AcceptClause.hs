module Sqel.Class.AcceptClause where

import Generics.SOP (NP (Nil, (:*)))

import Sqel.Class.AcceptFrag (AcceptFragment (acceptFragment), AcceptFragments (acceptFragments))
import Sqel.Class.ClauseResult (ClauseDecoders, ClauseResult (clauseResult))
import Sqel.Data.Clause (Clause (Clause), ClauseArgs (FragsOnly, FragsP), ClauseParam (ClauseParam))
import Sqel.Data.ClauseConfig (ClauseFieldsFor, ClauseResultFor)
import Sqel.Data.Codec (Decoder)
import Sqel.Data.Fragment (Frag, Fragment)
import Sqel.Error.Clause (CheckClauseError)
import Sqel.Kind.Maybe (MaybeD)
import Sqel.Kind.NormalizeFrags (InvalidFrags, NormalizeFrags (normalizeFrags))
import Sqel.Kind.ResultDds (ResultTypes)
import Sqel.SOP.Error (Quoted)

data Mult = Unclear | Multi

type FragMultiplicity :: Type -> Mult
type family FragMultiplicity field where
  FragMultiplicity [_] = 'Multi
  FragMultiplicity (NonEmpty _) = 'Multi
  FragMultiplicity _ = 'Unclear

------------------------------------------------------------------------------------------------------------------------

type AcceptClause :: ∀ {ext} . Void -> Type -> Type -> Mult -> [Frag ext] -> Type -> Constraint
class AcceptClause error tag clause mult frags fields where
  acceptClause :: NP Fragment frags -> fields

instance AcceptClause error tag clause 'Unclear '[] () where
  acceptClause Nil = ()

instance (
    AcceptFragment clause field frag
  ) => AcceptClause error tag clause 'Unclear '[frag] field where
    acceptClause (f :* Nil) = acceptFragment @clause f

instance (
    AcceptFragments clause frags field f
  ) => AcceptClause error tag clause 'Multi frags (f field) where
    acceptClause = acceptFragments @clause

------------------------------------------------------------------------------------------------------------------------

-- TODO add fields to error message
type AcceptClauseOrError :: Type -> Type -> Type -> Type -> Maybe [Type] -> Constraint
class AcceptClauseOrError tag clause expr fields result | clause expr -> result where
  acceptClauseOrError :: expr -> (fields, ClauseDecoders result)

instance (
    NormalizeFrags InvalidFrags expr frags,
    error ~ CheckClauseError tag clause frags,
    mult ~ FragMultiplicity fields,
    isResult ~ ClauseResultFor clause,
    result ~ ResultTypes clause frags,
    ClauseResult isResult frags result,
    AcceptClause error tag clause mult frags fields
  ) => AcceptClauseOrError tag clause expr fields result where
    acceptClauseOrError expr =
      (acceptClause @error @tag @clause @mult frags, clauseResult @isResult @frags frags)
      where
        frags = normalizeFrags @InvalidFrags expr

------------------------------------------------------------------------------------------------------------------------

-- TODO this is only relevant in smart constructors, so probably fine to just get an instance error
type ArgsError :: Maybe Type -> Type -> k
type family ArgsError param fields where
  ArgsError 'Nothing (ClauseParam _ param) =
    TypeError ("The clause takes a parameter of type " <> Quoted param <> ", but none was passed.")
  ArgsError 'Nothing _ = ()
  ArgsError ('Just param) (ClauseParam _ param) = ()
  ArgsError ('Just param) (ClauseParam _ cparam) =
    TypeError (ToErrorMessage ("A parameter of type " <> Quoted param <> " was given," %
    "but the clause takes a " <> Quoted cparam <> "."))
  ArgsError ('Just _) _ =
    TypeError (ToErrorMessage ("A parameter was given, but the clause doesn't support it."))

type AcceptClauseArgs :: Void -> Type -> Type -> Type -> Maybe Type -> Type -> Maybe [Type] -> Constraint
class AcceptClauseArgs error tag clause expr param fields result | clause expr -> result where
  acceptClauseArgs :: ClauseArgs expr param -> (fields, ClauseDecoders result)

instance (
    AcceptClauseOrError tag clause expr fields result
  ) => AcceptClauseArgs error tag clause expr 'Nothing fields result where
    acceptClauseArgs (FragsOnly expr) =
      acceptClauseOrError @tag @clause @expr @fields expr

instance (
    AcceptClauseOrError tag clause expr fields result
  ) => AcceptClauseArgs error tag clause expr ('Just param) (ClauseParam fields param) result where
    acceptClauseArgs (FragsP expr param) =
      (ClauseParam fields param, result)
      where
        (fields, result) = acceptClauseOrError @tag @clause @expr @fields expr

------------------------------------------------------------------------------------------------------------------------

type MkClause :: Type -> Type -> Type -> Maybe Type -> Maybe [Type] -> Constraint
class MkClause tag clause expr param result | clause expr -> result where
  mkClause :: ClauseArgs expr param -> (Clause tag clause, MaybeD (NP Decoder) result)

instance (
    fields ~ ClauseFieldsFor tag clause,
    NormalizeFrags InvalidFrags expr frags,
    error ~ ArgsError param fields,
    AcceptClauseArgs error tag clause expr param fields result
  ) => MkClause tag clause expr param result where
    mkClause args =
      first Clause (acceptClauseArgs @error @tag @clause args)
