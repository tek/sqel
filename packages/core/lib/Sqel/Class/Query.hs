module Sqel.Class.Query where

import Generics.SOP (All, NP (Nil, (:*)), SListI, hcpure, hmap)

import Sqel.Class.Check (Checked (checked), unchecked)
import Sqel.Class.ReifySqel (ReifySqel, sqel)
import Sqel.Data.Dd (DdK)
import Sqel.Data.Fragment (Fragment (Fragment))
import Sqel.Data.Fragments (
  Fragments (Fragments),
  ProjectionFragment (ProjectionFragment),
  ProjectionFragments (ProjectionFragments),
  QueryFragment (NoQueryFragment, QueryFragment),
  TableFragment (TableFragment),
  TableFragments (TableFragments),
  )
import Sqel.Data.Sqel (SqelFor)
import Sqel.Kind.Maybe (MaybeD (JustD, NothingD))
import Sqel.SOP.HasGeneric (BoolVal (boolVal))

type IsMulti :: [k] -> Bool
type family IsMulti tables where
  IsMulti (_ : _ : _) = 'True
  IsMulti _ = 'False

type NoQueryDd :: ∀ {ext} . Type -> [DdK ext] -> Constraint
class NoQueryDd tag tables where
  noQueryDd :: Fragments tag 'Nothing tables '[]

instance (
    All (ReifySqel tag) tables,
    multi ~ IsMulti tables,
    BoolVal multi
  ) => NoQueryDd tag tables where
    noQueryDd =
      Fragments NoQueryFragment (TableFragments tables) (ProjectionFragments Nil) multi
      where
        multi = boolVal @multi
        tables = hcpure (Proxy @(ReifySqel tag)) tableSpine
        tableSpine :: ReifySqel tag s => TableFragment tag s
        tableSpine = TableFragment (Fragment sqel)

type QueryDd :: ∀ {ext} . Type -> DdK ext -> [DdK ext] -> Constraint
class QueryDd tag query tables where
  queryDd :: Fragments tag ('Just query) tables '[]

instance (
    ReifySqel tag query,
    Checked tables tag query,
    NoQueryDd tag tables
  ) => QueryDd tag query tables where
    queryDd =
      Fragments (QueryFragment (checked @tables sqel)) tables proj multi
      where
        Fragments NoQueryFragment tables proj multi = noQueryDd @tag @tables

withQueryDd ::
  ∀ tag query tables a .
  QueryDd tag query tables =>
  (Fragments tag ('Just query) tables '[] -> a) ->
  a
withQueryDd f =
  f (queryDd @tag @query @tables)

type FragmentsDd :: ∀ {ext} . Type -> Maybe (DdK ext) -> [DdK ext] -> Constraint
class FragmentsDd tag query tables where
  fragmentsDd :: Fragments tag query tables '[]

instance (
    NoQueryDd tag tables
  ) => FragmentsDd tag 'Nothing tables where
    fragmentsDd = noQueryDd

instance (
    QueryDd tag query tables
  ) => FragmentsDd tag ('Just query) tables where
    fragmentsDd = queryDd

withFragmentsDd ::
  ∀ tag query tables a .
  FragmentsDd tag query tables =>
  (Fragments tag query tables '[] -> a) ->
  a
withFragmentsDd f =
  f (fragmentsDd @tag @query @tables)

type NoQuerySqel :: Type -> [DdK ext] -> Constraint
class NoQuerySqel tag tables where
  noQuerySqel :: NP (SqelFor tag) tables -> Fragments tag 'Nothing tables '[]

instance (
    SListI tables,
    multi ~ IsMulti tables,
    BoolVal multi
  ) => NoQuerySqel tag tables where
    noQuerySqel tables =
      Fragments NoQueryFragment (TableFragments (hmap tableSpine tables)) (ProjectionFragments Nil) multi
      where
        multi = boolVal @multi
        tableSpine :: SqelFor tag s -> TableFragment tag s
        tableSpine t = TableFragment (unchecked t)

type QuerySqel :: Type -> DdK ext -> [DdK ext] -> Constraint
class QuerySqel tag query tables where
  querySqel :: SqelFor tag query -> NP (SqelFor tag) tables -> Fragments tag ('Just query) tables '[]

instance (
    Checked tables tag query,
    NoQuerySqel tag tables
  ) => QuerySqel tag query tables where
    querySqel query tables =
      Fragments (QueryFragment (checked @tables query)) tableSpines proj multi
      where
        Fragments NoQueryFragment tableSpines proj multi = noQuerySqel tables

type FragmentsSqel :: ∀ {ext} . Type -> Maybe (DdK ext) -> [DdK ext] -> Constraint
class FragmentsSqel tag query tables where
  fragmentsSqel :: MaybeD (SqelFor tag) query -> NP (SqelFor tag) tables -> Fragments tag query tables '[]

instance (
    NoQuerySqel tag tables
  ) => FragmentsSqel tag 'Nothing tables where
    fragmentsSqel NothingD = noQuerySqel

instance (
    QuerySqel tag query tables
  ) => FragmentsSqel tag ('Just query) tables where
    fragmentsSqel (JustD query) = querySqel query

withProjection ::
  ∀ tag query tables projs proj .
  Checked tables tag proj =>
  SqelFor tag proj ->
  Fragments tag query tables projs ->
  Fragments tag query tables (proj : projs)
withProjection proj (Fragments qs ts (ProjectionFragments ps) m) =
  Fragments qs ts (ProjectionFragments (ProjectionFragment (checked @tables proj) :* ps)) m
