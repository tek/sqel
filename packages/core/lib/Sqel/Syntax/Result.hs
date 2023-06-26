module Sqel.Syntax.Result where

import Sqel.Build.Sql (BuildSql (buildSql))
import Sqel.Build.Statement (BuildStatement (buildStatement))
import Sqel.Data.Clause (ClauseK, Clauses (Clauses))
import Sqel.Data.Dd (DdK)
import Sqel.Data.Fragment (Frag (Frag), Frag0 (Frag0), Fragment)
import Sqel.Data.Fragments (Fragments, maybeQuery)
import Sqel.Data.Spine (SpineSort (SpineTable))
import Sqel.Data.Sql (Sql)
import Sqel.Data.Statement (Statement)
import Sqel.Dd (DdType, MaybeDdType)
import Sqel.Fragment (getFragment)
import Sqel.Kind.Maybe (MaybeD (JustD))

-- TODO would be nice if both @table@ and @table_@ could return Fragment.
-- Right now @table_@ uses Fragments to signal that there's no query, I guess because using @'Just table@ for the query
-- with @table@ would cause overlap.

-- | Just for the purpose of nicer error messages.
--
-- This is for a statement that only uses one @Sqel@, both as query and table, like @insertInto@.
type TableQResult :: ∀ {ext} . Type -> Type -> DdK ext -> [ClauseK ext] -> Constraint
class TableQResult result tag table cs where
  tableQResult :: Fragment ('Frag ('Frag0 tag 'SpineTable table root comp)) -> Clauses tag cs a -> result

instance BuildSql tag cs => TableQResult Sql tag table cs where
  tableQResult _ (Clauses cs a) = buildSql False (Clauses cs a)

instance (
    BuildStatement tag ('Just table) result cs,
    q ~ DdType table
  ) => TableQResult (Statement q result) tag table cs where
    tableQResult frag (Clauses cs a) =
      buildStatement @tag @('Just table) @result @cs False (Clauses cs a) (JustD (getFragment frag))

-- | Just for the purpose of nicer error messages.
--
-- This is for a statement that only uses one @Sqel@ as a table, with no query, like @createTable@.
type TableResult :: ∀ {ext} . Type -> Type -> DdK ext -> [ClauseK ext] -> Constraint
class TableResult result tag table cs where
  tableResult :: Fragments tag 'Nothing '[table] '[] -> Clauses tag cs a -> result

instance BuildSql tag cs => TableResult Sql tag table cs where
  tableResult _ (Clauses cs a) = buildSql False (Clauses cs a)

instance (
    BuildStatement tag ('Nothing :: Maybe (DdK ext)) result cs
  ) => TableResult (Statement () result) tag table cs where
    tableResult frags (Clauses cs a) =
      buildStatement @tag @'Nothing @result @cs frags.multi (Clauses cs a) (maybeQuery frags)

-- | Just for the purpose of nicer error messages.
--
-- This is for a statement that uses a query and a single table, without projections.
type QueryResult ::
  ∀ {ext} .
  Type ->
  Type ->
  DdK ext ->
  DdK ext ->
  [ClauseK ext] ->
  Constraint
class QueryResult result tag query table cs where
  queryResult :: Fragments tag ('Just query) '[table] '[] -> Clauses tag cs a -> result

instance BuildSql tag cs => QueryResult Sql tag query table cs where
  queryResult frags (Clauses cs a) = buildSql frags.multi (Clauses cs a)

instance (
    BuildStatement tag ('Just query) result cs,
    q ~ DdType query
  ) => QueryResult (Statement q result) tag query table cs where
    queryResult frags (Clauses cs a) =
      buildStatement @tag @('Just query) @result @cs frags.multi (Clauses cs a) (maybeQuery frags)

-- | Just for the purpose of nicer error messages.
--
-- This is for a statement that uses a query and more than one table, without projections.
type JoinResult ::
  ∀ {ext} .
  Type ->
  Type ->
  DdK ext ->
  [DdK ext] ->
  [ClauseK ext] ->
  Constraint
class JoinResult result tag query tables cs where
  joinResult :: Fragments tag ('Just query) tables '[] -> Clauses tag cs a -> result

instance BuildSql tag cs => JoinResult Sql tag query tables cs where
  joinResult frags (Clauses cs a) = buildSql frags.multi (Clauses cs a)

instance (
    BuildStatement tag ('Just query) result cs,
    q ~ DdType query
  ) => JoinResult (Statement q result) tag query tables cs where
    joinResult frags (Clauses cs a) =
      buildStatement @tag @('Just query) @result @cs frags.multi (Clauses cs a) (maybeQuery frags)

-- | Just for the purpose of nicer error messages.
--
-- This is for a statement that uses all fragment sorts.
type StatementResult ::
  ∀ {ext} .
  Type ->
  Type ->
  Maybe (DdK ext) ->
  [DdK ext] ->
  [DdK ext] ->
  [ClauseK ext] ->
  Constraint
class StatementResult result tag query tables projs cs where
  statementResult :: Fragments tag query tables projs -> Clauses tag cs a -> result

instance BuildSql tag cs => StatementResult Sql tag query tables projs cs where
  statementResult frags (Clauses cs a) = buildSql frags.multi (Clauses cs a)

instance (
    BuildStatement tag query result cs,
    q ~ MaybeDdType query
  ) => StatementResult (Statement q result) tag query tables projs cs where
    statementResult frags (Clauses cs a) =
      buildStatement @tag @query @result @cs frags.multi (Clauses cs a) (maybeQuery frags)

type DoResult :: ∀ {ext} . Type -> Type -> [ClauseK ext] -> Type -> Type -> Constraint
class DoResult frags tag cs a result where
  doResult :: frags -> Clauses tag cs a -> result

instance (
    TableResult result tag table cs
  ) => DoResult (Fragments tag 'Nothing '[table] '[]) tag cs a result where
    doResult = tableResult

instance (
    TableQResult result tag table cs
  ) => DoResult (Fragment ('Frag ('Frag0 tag 'SpineTable table 'True comp))) tag cs a result where
    doResult = tableQResult

instance (
    QueryResult result tag query table cs
  ) => DoResult (Fragments tag ('Just query) '[table] '[]) tag cs a result where
    doResult = queryResult

instance (
    JoinResult result tag query (t0 : t1 : tables) cs
  ) => DoResult (Fragments tag ('Just query) (t0 : t1 : tables) '[]) tag cs a result where
    doResult = joinResult

instance {-# overlappable #-} (
    StatementResult result tag query tables projs cs
  ) => DoResult (Fragments tag query tables projs) tag cs a result where
    doResult = statementResult
