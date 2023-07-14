{-# language QualifiedDo #-}

module Sqel.Statement.Common where

import Sqel.Build.Sql (BuildClause)
import Sqel.Class.Check (Check1)
import qualified Sqel.Clauses as Clauses
import Sqel.Clauses (deleteFrom, doUpdateSet, from, insertInto, onConflict, returning, select, values, where_)
import Sqel.Data.Drop (Cascade (Cascade), Drop (Drop))
import Sqel.Data.Sqel (SqelFor)
import Sqel.Data.Statement (Statement)
import Sqel.Dd (DdType, IsComp)
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
import Sqel.Syntax.Fragments (query, table, table_)
import qualified Sqel.Syntax.Monad as S

createTable ::
  ∀ tag table .
  BuildClause tag CreateTable =>
  SqelFor tag table ->
  Statement '[DdType table] () ()
createTable s = S.do
  fs <- table_ s
  Clauses.createTable fs.table

-- | drop table if exists <x>
dropTable ::
  ∀ tag table .
  BuildClause tag DropTable =>
  SqelFor tag table ->
  Statement '[DdType table] () ()
dropTable s = S.do
  fs <- table_ s
  Clauses.dropTable fs.table (Drop True Nothing)

-- | drop table if exists <x> cascade
dropTableCascade ::
  ∀ tag table .
  BuildClause tag DropTable =>
  SqelFor tag table ->
  Statement '[DdType table] () ()
dropTableCascade s = S.do
  fs <- table_ s
  Clauses.dropTable fs.table (Drop True (Just Cascade))

-- TODO this could require that the containing table and a projection is supplied to prove that it's a composite type
createType ::
  ∀ tag t .
  IsComp t ~ 'True =>
  BuildClause tag CreateType =>
  SqelFor tag t ->
  Statement '[DdType t] () ()
createType s = S.do
  fs <- table_ s
  Clauses.createType fs.table

selectWhere ::
  ∀ tag query table .
  Check1 table query =>
  BuildClause tag Select =>
  BuildClause tag From =>
  BuildClause tag Where =>
  SqelFor tag query ->
  SqelFor tag table ->
  Statement '[DdType table] (DdType query) (DdType table)
selectWhere q t = S.do
  c <- query q t
  select c.table
  from c.table
  where_ c.query

selectAll ::
  ∀ tag table .
  BuildClause tag Select =>
  BuildClause tag From =>
  SqelFor tag table ->
  Statement '[DdType table] () (DdType table)
selectAll s = S.do
  fs <- table_ s
  select fs.table
  from fs.table

insert ::
  ∀ tag table .
  BuildClause tag InsertInto =>
  BuildClause tag Values =>
  SqelFor tag table ->
  Statement '[DdType table] (DdType table) ()
insert s = S.do
  t <- table s
  insertInto t
  values t

upsert ::
  ∀ tag table .
  BuildClause tag InsertInto =>
  BuildClause tag Values =>
  BuildClause tag OnConflict =>
  BuildClause tag DoUpdateSet =>
  SqelFor tag table ->
  Statement '[DdType table] (DdType table) ()
upsert s = S.do
  t <- table s
  insertInto t
  values t
  onConflict t
  doUpdateSet t

delete ::
  ∀ tag query table .
  Check1 table query =>
  BuildClause tag DeleteFrom =>
  BuildClause tag Where =>
  BuildClause tag Returning =>
  SqelFor tag query ->
  SqelFor tag table ->
  Statement '[DdType table] (DdType query) (DdType table)
delete q t = S.do
  fs <- query q t
  deleteFrom fs.table
  where_ fs.query
  returning fs.table

deleteAll ::
  ∀ tag table .
  BuildClause tag DeleteFrom =>
  BuildClause tag Returning =>
  SqelFor tag table ->
  Statement '[DdType table] () (DdType table)
deleteAll t = S.do
  fs <- table_ t
  deleteFrom fs.table
  returning fs.table
