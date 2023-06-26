{-# language QualifiedDo #-}

module Sqel.Statement.Common where

import Sqel.Class.Check (Checked1)
import qualified Sqel.Clauses as Clauses
import Sqel.Clauses (deleteFrom, doUpdateSet, from, insertInto, onConflict, returning, select, values, where_)
import Sqel.Data.Clause (ClauseK (ClauseK, ResultK))
import Sqel.Data.Drop (Cascade (Cascade), Drop (Drop))
import Sqel.Data.Sqel (SqelFor)
import Sqel.Dd (IsComp)
import Sqel.Default (
  CreateTable,
  CreateType,
  DeleteFrom,
  DoUpdateSet,
  DropTable,
  From,
  InsertInto,
  OnConflict,
  Returning,
  Select,
  Values,
  Where,
  )
import Sqel.Syntax.Fragments (query1, table, table_)
import qualified Sqel.Syntax.Monad as S
import Sqel.Syntax.Result (QueryResult, TableQResult, TableResult)

createTable ::
  ∀ result tag table .
  TableResult result tag table '[ 'ClauseK CreateTable] =>
  SqelFor tag table ->
  result
createTable s = S.do
  fs <- table_ s
  Clauses.createTable fs.table

-- | drop table if exists <x>
dropTable ::
  ∀ result tag table .
  TableResult result tag table '[ 'ClauseK DropTable] =>
  SqelFor tag table ->
  result
dropTable s = S.do
  fs <- table_ s
  Clauses.dropTable fs.table (Drop True Nothing)

-- | drop table if exists <x> cascade
dropTableCascade ::
  ∀ result tag table .
  TableResult result tag table '[ 'ClauseK DropTable] =>
  SqelFor tag table ->
  result
dropTableCascade s = S.do
  fs <- table_ s
  Clauses.dropTable fs.table (Drop True (Just Cascade))

-- TODO this could require that the containing table and a projection is supplied to prove that it's a composite type
createType ::
  ∀ result tag t .
  TableResult result tag t '[ 'ClauseK CreateType] =>
  IsComp t ~ 'True =>
  SqelFor tag t ->
  result
createType s = S.do
  fs <- table_ s
  Clauses.createType fs.table

selectWhere ::
  ∀ result tag query table .
  Checked1 table tag query =>
  QueryResult result tag query table ['ResultK Select table, 'ClauseK From, 'ClauseK Where] =>
  SqelFor tag query ->
  SqelFor tag table ->
  result
selectWhere q t = S.do
  c <- query1 q t
  select c.table
  from c.table
  where_ c.query

selectAll ::
  ∀ result tag table .
  TableResult result tag table ['ResultK Select table, 'ClauseK From] =>
  SqelFor tag table ->
  result
selectAll s = S.do
  fs <- table_ s
  select fs.table
  from fs.table

insert ::
  ∀ result tag table .
  TableQResult result tag table ['ClauseK InsertInto, 'ClauseK Values] =>
  SqelFor tag table ->
  result
insert s = S.do
  t <- table s
  insertInto t
  values t

upsert ::
  ∀ result tag table .
  TableQResult result tag table ['ClauseK InsertInto, 'ClauseK Values, 'ClauseK OnConflict, 'ClauseK DoUpdateSet] =>
  SqelFor tag table ->
  result
upsert s = S.do
  t <- table s
  insertInto t
  values t
  onConflict t
  doUpdateSet t

delete ::
  ∀ result tag query table .
  Checked1 table tag query =>
  QueryResult result tag query table ['ClauseK DeleteFrom, 'ClauseK Where, 'ResultK Returning table] =>
  SqelFor tag query ->
  SqelFor tag table ->
  result
delete q t = S.do
  fs <- query1 q t
  deleteFrom fs.table
  where_ fs.query
  returning fs.table

deleteAll ::
  ∀ result tag table .
  TableResult result tag table ['ClauseK DeleteFrom, 'ResultK Returning table] =>
  SqelFor tag table ->
  result
deleteAll t = S.do
  fs <- table_ t
  deleteFrom fs.table
  returning fs.table
