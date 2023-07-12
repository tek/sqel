module Sqel.Build where

import Generics.SOP (NP)

import Sqel.Build.Statement (BuildStatement (buildStatement))
import Sqel.Class.Query (FragmentsSqel (fragmentsSqel))
import Sqel.Class.ReifySqel (ReifySqelFor, ReifySqels, sqel, sqels)
import Sqel.Data.Clause (Clauses)
import Sqel.Data.Dd (Dd)
import Sqel.Data.Fragments (Fragments)
import Sqel.Data.Sqel (SqelFor)
import Sqel.Data.Statement (Statement)
import Sqel.Dd (DdTypes, MaybeDdType)
import Sqel.Default (Def, Sqel)
import Sqel.Kind.Maybe (MaybeD (JustD, NothingD))
import Sqel.Kind.ResultTuple (ResultTuple)

type Build :: ∀ {ext} . Type -> Type -> Maybe (Dd ext) -> [Dd ext] -> [Type] -> Maybe [Type] -> Constraint
class Build tag result query tables cs results where
  build ::
    MaybeD (SqelFor tag) query ->
    NP (SqelFor tag) tables ->
    (Fragments tag query tables '[] -> Clauses tag cs results ()) ->
    Statement (DdTypes tables) (MaybeDdType query) result

instance (
    FragmentsSqel tag query tables,
    BuildStatement tag (DdTypes tables) query cs results result
  ) => Build tag result query tables cs results where
  build query tables use =
    buildStatement @tag @(DdTypes tables) @query @cs @results frags.multi (use frags) query
    where
      frags = fragmentsSqel @tag query tables

type BuildK ::
  ∀ {ext} . Type ->
  Maybe (Dd ext) ->
  [Dd ext] ->
  Type ->
  [Type] ->
  Maybe [Type] ->
  Constraint
class BuildK tag query tables result cs results where
  buildK ::
    (Fragments tag query tables '[] -> Clauses tag cs results ()) ->
    Statement (DdTypes tables) (MaybeDdType query) result

instance (
    ReifySqelFor tag query,
    ReifySqels tag tables,
    Build tag result ('Just query) tables cs results
  ) => BuildK tag ('Just query) tables result cs results where
    buildK = build (JustD sqel) sqels

instance (
    ReifySqels tag tables,
    Build tag result 'Nothing tables cs results
  ) => BuildK tag 'Nothing tables result cs results where
    buildK = build NothingD sqels

buildAs ::
  ∀ result query tables cs results .
  Build Def result query tables cs results =>
  MaybeD Sqel query ->
  NP Sqel tables ->
  (Fragments Def query tables '[] -> Clauses Def cs results ()) ->
  Statement (DdTypes tables) (MaybeDdType query) result
buildAs =
  build @Def @result

buildKAs ::
  ∀ query tables result cs results .
  BuildK Def query tables result cs results =>
  (Fragments Def query tables '[] -> Clauses Def cs results ()) ->
  Statement (DdTypes tables) (MaybeDdType query) result
buildKAs =
  buildK

buildTuple ::
  ∀ query tables tag cs results .
  Build tag (ResultTuple results) query tables cs results =>
  MaybeD (SqelFor tag) query ->
  NP (SqelFor tag) tables ->
  (Fragments tag query tables '[] -> Clauses tag cs results ()) ->
  Statement (DdTypes tables) (MaybeDdType query) (ResultTuple results)
buildTuple =
  build @tag @(ResultTuple results)

buildKTuple ::
  ∀ query tables cs results .
  BuildK Def query tables (ResultTuple results) cs results =>
  (Fragments Def query tables '[] -> Clauses Def cs results ()) ->
  Statement (DdTypes tables) (MaybeDdType query) (ResultTuple results)
buildKTuple =
  buildK @Def
