module Sqel.Data.ClauseConfig where

import Sqel.Data.Spine (SpineSort)

type ClauseConfig :: Type -> Type
data ClauseConfig tag =
  ClauseConfig {
    result :: Bool,
    sorts :: [SpineSort],
    fields :: Type,
    keyword :: Symbol,
    literal :: Maybe Type
  }

type ClauseConfigFor :: ∀ tag -> Type -> ClauseConfig tag
type family ClauseConfigFor tag clause

type ClauseResultOf :: ClauseConfig tag -> Bool
type family ClauseResultOf config where
  ClauseResultOf ('ClauseConfig r _ _ _ _) = r

type ClauseResultFor :: Type -> Bool
type family ClauseResultFor clause where
  ClauseResultFor clause = ClauseResultOf (ClauseConfigFor () clause)

type ClauseSortsOf :: ClauseConfig tag -> [SpineSort]
type family ClauseSortsOf config where
  ClauseSortsOf ('ClauseConfig _ ss _ _ _) = ss

type ClauseSortsFor :: Type -> [SpineSort]
type family ClauseSortsFor clause where
  ClauseSortsFor clause = ClauseSortsOf (ClauseConfigFor () clause)

type ClauseFieldsOf :: ClauseConfig tag -> Type
type family ClauseFieldsOf config where
  ClauseFieldsOf ('ClauseConfig _ _ fs _ _) = fs

type ClauseFieldsFor :: Type -> Type -> Type
type family ClauseFieldsFor tag clause where
  ClauseFieldsFor tag clause = ClauseFieldsOf (ClauseConfigFor tag clause)

type ClauseKeywordOf :: ClauseConfig tag -> Symbol
type family ClauseKeywordOf config where
  ClauseKeywordOf ('ClauseConfig _ _ _ desc _) = desc

type ClauseKeywordFor :: Type -> Symbol
type family ClauseKeywordFor clause where
  ClauseKeywordFor clause = ClauseKeywordOf (ClauseConfigFor () clause)

type ClauseLiteralOf :: ClauseConfig tag -> Maybe Type
type family ClauseLiteralOf config where
  ClauseLiteralOf ('ClauseConfig _ _ _ _ lit) = lit

type ClauseLiteralFor :: Type -> Maybe Type
type family ClauseLiteralFor clause where
  ClauseLiteralFor clause = ClauseLiteralOf (ClauseConfigFor () clause)
