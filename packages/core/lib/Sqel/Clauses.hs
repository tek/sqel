module Sqel.Clauses where

import Generics.SOP (NP (Nil, (:*)))

import Sqel.Class.AcceptClause (MkClause (mkClause))
import Sqel.Data.Clause (ClauseArgs (FragsOnly, FragsP), Clauses (Clauses))
import Sqel.Data.Drop (Drop)
import Sqel.Data.Order (Order)
import Sqel.Default (
  CreateTable,
  CreateType,
  DeleteFrom,
  DoUpdateSet,
  DropTable,
  From,
  InsertInto,
  Join,
  Limit,
  Offset,
  On,
  OnConflict,
  OrderBy,
  Returning,
  Select,
  Values,
  Where,
  )

type ClauseCon :: Type -> Type
type ClauseCon clause =
  ∀ tag expr result .
  MkClause tag clause expr 'Nothing result =>
  expr ->
  Clauses tag '[clause] result ()

type ClausePCon :: Type -> Type -> Type
type ClausePCon clause param =
  ∀ tag expr result .
  MkClause tag clause expr ('Just param) result =>
  expr ->
  param ->
  Clauses tag '[clause] result ()

clause :: ClauseCon clause
clause expr =
  Clauses (c :* Nil) r ()
  where
    (c, r) = mkClause (FragsOnly expr)

clauseP :: ClausePCon clause param
clauseP expr param =
  Clauses (c :* Nil) r ()
  where
    (c, r) = mkClause (FragsP expr param)

select :: ClauseCon Select
select = clause

deleteFrom :: ClauseCon DeleteFrom
deleteFrom = clause

from :: ClauseCon From
from = clause

join :: ClauseCon Join
join = clause

on :: ClauseCon On
on = clause

where_ :: ClauseCon Where
where_ = clause

insertInto :: ClauseCon InsertInto
insertInto = clause

values :: ClauseCon Values
values = clause

onConflict :: ClauseCon OnConflict
onConflict = clause

doUpdateSet :: ClauseCon DoUpdateSet
doUpdateSet = clause

returning :: ClauseCon Returning
returning = clause

createTable :: ClauseCon CreateTable
createTable = clause

dropTable :: ClausePCon DropTable Drop
dropTable = clauseP

createType :: ClauseCon CreateType
createType = clause

orderBy :: ClausePCon OrderBy Order
orderBy = clauseP

limit :: ClauseCon Limit
limit = clause

offset :: ClauseCon Offset
offset = clause
