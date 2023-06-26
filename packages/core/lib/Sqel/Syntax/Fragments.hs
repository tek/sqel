module Sqel.Syntax.Fragments where

import Generics.SOP (NP (Nil, (:*)))

import Sqel.Class.Check (Checked)
import Sqel.Class.Query (FragmentsDd (fragmentsDd), FragmentsSqel (fragmentsSqel), withProjection)
import Sqel.Class.ReifySqel (ReifySqel, sqel)
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
  Clauses tag '[] (Fragments tag query tables '[])
queryKWith =
  Clauses Nil frags
  where
    frags = fragmentsDd @tag @query @tables

queryK ::
  ∀ query tables tag .
  FragmentsDd tag ('Just query) tables =>
  Clauses tag '[] (Fragments tag ('Just query) tables '[])
queryK =
  queryKWith

queryWith ::
  ∀ tag query tables .
  FragmentsSqel tag query tables =>
  MaybeD (SqelFor tag) query ->
  NP (SqelFor tag) tables ->
  Clauses tag '[] (Fragments tag query tables '[])
queryWith q t =
  Clauses Nil frags
  where
    frags = fragmentsSqel q t

-- TODO use tuples here instead of NP
query ::
  ∀ tag query tables .
  FragmentsSqel tag ('Just query) tables =>
  SqelFor tag query ->
  NP (SqelFor tag) tables ->
  Clauses tag '[] (Fragments tag ('Just query) tables '[])
query =
  queryWith . JustD

query1K ::
  ∀ query table tag .
  FragmentsDd tag ('Just query) '[table] =>
  Clauses tag '[] (Fragments tag ('Just query) '[table] '[])
query1K =
  queryKWith

query1 ::
  ∀ tag query table .
  FragmentsSqel tag ('Just query) '[table] =>
  SqelFor tag query ->
  SqelFor tag table ->
  Clauses tag '[] (Fragments tag ('Just query) '[table] '[])
query1 q t =
  query q (t :* Nil)

tableK_ ::
  ∀ table tag .
  FragmentsDd tag 'Nothing '[table] =>
  Clauses tag '[] (Fragments tag 'Nothing '[table] '[])
tableK_ =
  queryKWith @'Nothing @'[table]

table_ ::
  ∀ tag table .
  FragmentsSqel tag 'Nothing '[table] =>
  SqelFor tag table ->
  Clauses tag '[] (Fragments tag 'Nothing '[table] '[])
table_ t =
  queryWith NothingD (t :* Nil)

tableK ::
  ∀ table tag .
  ReifySqel tag table =>
  Clauses tag '[] (Fragment ('Frag ('Frag0 tag 'SpineTable table 'True (IsComp table))))
tableK =
  Clauses Nil (Fragment sqel)

table ::
  ∀ tag table .
  SqelFor tag table ->
  Clauses tag '[] (Fragment ('Frag ('Frag0 tag 'SpineTable table 'True (IsComp table))))
table t =
  Clauses Nil (Fragment t)

tablesK ::
  ∀ tables tag .
  FragmentsDd tag 'Nothing tables =>
  Clauses tag '[] (Fragments tag 'Nothing tables '[])
tablesK =
  queryKWith @'Nothing @tables

tables ::
  ∀ tag tables .
  FragmentsSqel tag 'Nothing tables =>
  NP (SqelFor tag) tables ->
  Clauses tag '[] (Fragments tag 'Nothing tables '[])
tables =
  queryWith NothingD

project ::
  ∀ tag query tables proj projs .
  Checked tables tag proj =>
  SqelFor tag proj ->
  Clauses tag '[] (Fragments tag query tables projs) ->
  Clauses tag '[] (Fragments tag query tables (proj : projs))
project proj (Clauses Nil frags) =
  Clauses Nil (withProjection proj frags)
