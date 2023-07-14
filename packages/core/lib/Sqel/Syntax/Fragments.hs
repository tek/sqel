module Sqel.Syntax.Fragments where

import Generics.SOP (NP (Nil, (:*)))

import Sqel.Class.Check (Check)
import Sqel.Class.Query (
  FragmentsDd (fragmentsDd),
  FragmentsSqel (fragmentsSqel),
  NoQueryDd (noQueryDd),
  NoQuerySqel,
  withProjection,
  )
import Sqel.Class.ReifySqel (ReifySqelFor, sqel)
import Sqel.Class.TablesTuple (TablesTuple (tablesTuple))
import Sqel.Data.Clause (Clauses (Clauses))
import Sqel.Data.Fragment (Frag (Frag), Frag0 (Frag0), Fragment (Fragment))
import Sqel.Data.Fragments (Fragments)
import Sqel.Data.Spine (SpineSort (SpineTable))
import Sqel.Data.Sqel (SqelFor)
import Sqel.Dd (IsComp)
import Sqel.Kind.Maybe (MaybeD (JustD, NothingD))

queryKWith ::
  ∀ query tables tag .
  FragmentsDd tag query tables =>
  Clauses tag '[] 'Nothing (Fragments tag query tables '[])
queryKWith =
  Clauses Nil NothingD frags
  where
    frags = fragmentsDd @tag @query @tables

queryK ::
  ∀ query tables tag .
  FragmentsDd tag ('Just query) tables =>
  Clauses tag '[] 'Nothing (Fragments tag ('Just query) tables '[])
queryK =
  queryKWith

queryWith ::
  ∀ tag query tables .
  FragmentsSqel tag query tables =>
  MaybeD (SqelFor tag) query ->
  NP (SqelFor tag) tables ->
  Clauses tag '[] 'Nothing (Fragments tag query tables '[])
queryWith q t =
  Clauses Nil NothingD frags
  where
    frags = fragmentsSqel q t

query ::
  ∀ tag query texpr tables .
  TablesTuple tag texpr tables =>
  FragmentsSqel tag ('Just query) tables =>
  SqelFor tag query ->
  texpr ->
  Clauses tag '[] 'Nothing (Fragments tag ('Just query) tables '[])
query q ts =
  queryWith (JustD q) (tablesTuple ts)

query1K ::
  ∀ query table tag .
  FragmentsDd tag ('Just query) '[table] =>
  Clauses tag '[] 'Nothing (Fragments tag ('Just query) '[table] '[])
query1K =
  queryKWith

tableK_ ::
  ∀ table tag .
  NoQueryDd tag '[table] =>
  Clauses tag '[] 'Nothing (Fragments tag 'Nothing '[table] '[])
tableK_ =
  Clauses Nil NothingD frags
  where
    frags = noQueryDd @tag @'[table]

table_ ::
  ∀ tag table .
  SqelFor tag table ->
  Clauses tag '[] 'Nothing (Fragments tag 'Nothing '[table] '[])
table_ t =
  queryWith NothingD (t :* Nil)

tableK ::
  ∀ table tag .
  ReifySqelFor tag table =>
  Clauses tag '[] 'Nothing (Fragment ('Frag ('Frag0 tag 'SpineTable table 'True (IsComp table))))
tableK =
  Clauses Nil NothingD (Fragment sqel)

table ::
  ∀ tag table .
  SqelFor tag table ->
  Clauses tag '[] 'Nothing (Fragment ('Frag ('Frag0 tag 'SpineTable table 'True (IsComp table))))
table t =
  Clauses Nil NothingD (Fragment t)

tablesK ::
  ∀ tables tag .
  NoQueryDd tag tables =>
  Clauses tag '[] 'Nothing (Fragments tag 'Nothing tables '[])
tablesK =
  queryKWith @'Nothing @tables

tables ::
  ∀ tag texpr tables .
  TablesTuple tag texpr tables =>
  NoQuerySqel tag tables =>
  texpr ->
  Clauses tag '[] 'Nothing (Fragments tag 'Nothing tables '[])
tables =
  queryWith NothingD . tablesTuple

project ::
  ∀ tag proj query tables projs .
  Check tables proj =>
  SqelFor tag proj ->
  Clauses tag '[] 'Nothing (Fragments tag query tables projs) ->
  Clauses tag '[] 'Nothing (Fragments tag query tables (proj : projs))
project proj (Clauses Nil res frags) =
  Clauses Nil res (withProjection proj frags)
