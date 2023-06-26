module Sqel.Build where

import Generics.SOP (NP)

import Sqel.Build.Statement (BuildStatement (buildStatement))
import Sqel.Class.Query (FragmentsSqel (fragmentsSqel))
import Sqel.Class.ReifySqel (ReifySqel, ReifySqels, sqel, sqels)
import Sqel.Codec.Result (ClausesResult)
import Sqel.Data.Clause (ClauseK, Clauses)
import Sqel.Data.Dd (DdK)
import Sqel.Data.Fragments (Fragments)
import Sqel.Data.Sqel (SqelFor)
import Sqel.Data.Statement (Statement)
import Sqel.Dd (MaybeDdType)
import Sqel.Default (Def, Sqel)
import Sqel.Kind.Maybe (MaybeD (JustD, NothingD))
import Sqel.Kind.ResultTuple (ResultTuple)

type Build :: ∀ {ext} . Type -> Type -> Maybe (DdK ext) -> [DdK ext] -> [ClauseK ext] -> Constraint
class Build tag result query tables cs where
  build ::
    MaybeD (SqelFor tag) query ->
    NP (SqelFor tag) tables ->
    (Fragments tag query tables '[] -> Clauses tag cs ()) ->
    Statement (MaybeDdType query) result

instance (
    FragmentsSqel tag query tables,
    BuildStatement tag query result cs
  ) => Build tag result query tables cs where
  build query tables use =
    buildStatement @tag @query @result frags.multi (use frags) query
    where
      frags = fragmentsSqel @tag query tables

type BuildK ::
  ∀ {ext} . Type ->
  Maybe (DdK ext) ->
  [DdK ext] ->
  Type ->
  [ClauseK ext] ->
  Constraint
class BuildK tag query tables result cs where
  buildK :: (Fragments tag query tables '[] -> Clauses tag cs ()) -> Statement (MaybeDdType query) result

instance (
    ReifySqel tag query,
    ReifySqels tag tables,
    Build tag result ('Just query) tables cs
  ) => BuildK tag ('Just query) tables result cs where
    buildK = build (JustD sqel) sqels

instance (
    ReifySqels tag tables,
    Build tag result 'Nothing tables cs
  ) => BuildK tag 'Nothing tables result cs where
    buildK = build NothingD sqels

buildAs ::
  ∀ result query tables cs .
  Build Def result query tables cs =>
  MaybeD Sqel query ->
  NP Sqel tables ->
  (Fragments Def query tables '[] -> Clauses Def cs ()) ->
  Statement (MaybeDdType query) result
buildAs =
  build @Def @result

buildKAs ::
  ∀ query tables result cs .
  BuildK Def query tables result cs =>
  (Fragments Def query tables '[] -> Clauses Def cs ()) ->
  Statement (MaybeDdType query) result
buildKAs =
  buildK

buildTuple ::
  ∀ query tables tag cs .
  Build tag (ResultTuple (ClausesResult cs)) query tables cs =>
  MaybeD (SqelFor tag) query ->
  NP (SqelFor tag) tables ->
  (Fragments tag query tables '[] -> Clauses tag cs ()) ->
  Statement (MaybeDdType query) (ResultTuple (ClausesResult cs))
buildTuple =
  build @tag @(ResultTuple (ClausesResult cs))

buildKTuple ::
  ∀ query tables cs .
  BuildK Def query tables (ResultTuple (ClausesResult cs)) cs =>
  (Fragments Def query tables '[] -> Clauses Def cs ()) ->
  Statement (MaybeDdType query) (ResultTuple (ClausesResult cs))
buildKTuple =
  buildK @Def
