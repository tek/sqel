module Sqel.Error.Clause where

import Fcf (Pure)
import Type.Errors (DelayError, DelayErrorFcf, ErrorMessage, IfStuck)

import Sqel.Data.ClauseConfig (ClauseKeywordFor)
import Sqel.Data.Fragment (Frag)
import Sqel.SOP.Error (Quoted, QuotedType)

type ClauseDesc :: Type -> ErrorMessage
type family ClauseDesc clause where
  ClauseDesc clause = Quoted (ClauseKeywordFor clause) <> " clause"

type ClauseError :: Type -> Type -> [Frag ext] -> k
type family ClauseError tag clause frags

type GenericClause :: Type -> ErrorMessage
type GenericClause clause = "clause of type " <> QuotedType clause

type SuggestInstance :: ErrorMessage -> Type -> ErrorMessage
type family SuggestInstance tag clause where
  SuggestInstance tag clause =
    "You can define an instance to accept them:" %
    "instance AcceptClause error " <> tag <> " " <> clause <> " <desired fragments shape>"

type ErrorNoTag :: Type -> ErrorMessage
type family ErrorNoTag clause where
  ErrorNoTag clause =
    "These fragments are not supported for a " <> GenericClause clause <> "." %
    SuggestInstance (ToErrorMessage "<tag>") clause

type GeneralError :: Type -> Type -> ErrorMessage -> ErrorMessage
type family GeneralError tag clause desc where
  GeneralError tag clause desc =
    "The custom tag " <> QuotedType tag <> " does not support these fragments for a " <> desc <> "." %
    SuggestInstance ('ShowType tag) clause

type GeneralErrorWithDesc :: Type -> Type -> ErrorMessage -> k
type family GeneralErrorWithDesc tag clause desc where
  GeneralErrorWithDesc tag clause desc =
    IfStuck
    desc
    (DelayError (GeneralError tag clause (GenericClause clause)))
    (DelayErrorFcf (GeneralError tag clause desc))

type ErrorWithTag :: Type -> Type -> ErrorMessage -> k
type family ErrorWithTag tag clause custom where
  ErrorWithTag tag clause custom =
    IfStuck custom
    (GeneralErrorWithDesc tag clause (ClauseDesc clause))
    (DelayErrorFcf custom)

type CheckClauseError :: Type -> Type -> [Frag ext] -> k
type family CheckClauseError tag clause frags where
  CheckClauseError tag clause frags =
    IfStuck tag (DelayError (ErrorNoTag clause)) (Pure (ErrorWithTag tag clause (ClauseError tag clause frags)))
