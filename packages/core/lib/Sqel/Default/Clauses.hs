module Sqel.Default.Clauses where

import Prelude hiding (Set)

import Sqel.Data.Clause (ClauseParam)
import Sqel.Data.ClauseConfig (ClauseConfig (ClauseConfig), ClauseConfigFor)
import Sqel.Data.Drop (Drop)
import Sqel.Data.Field (CondField, Field, OrLiteral, PrimField, RootField, TypeField)
import Sqel.Data.Order (Order)
import Sqel.Data.Spine (SpineSort (SpineProj, SpineQuery, SpineTable))

data Select

type instance ClauseConfigFor tag Select =
  'ClauseConfig 'True '[ 'SpineTable, 'SpineProj] [Field tag] "select" 'Nothing

data DeleteFrom

type instance ClauseConfigFor tag DeleteFrom =
  'ClauseConfig 'False '[ 'SpineTable] (RootField tag) "delete from" 'Nothing

data From

type instance ClauseConfigFor tag From =
  'ClauseConfig 'False '[ 'SpineTable] (RootField tag) "from" 'Nothing

data Join

type instance ClauseConfigFor tag Join =
  'ClauseConfig 'False '[ 'SpineTable] (RootField tag) "join" 'Nothing

data InsertInto

type instance ClauseConfigFor tag InsertInto =
  'ClauseConfig 'False '[ 'SpineTable] (RootField tag) "insert into" 'Nothing

data Update

type instance ClauseConfigFor tag Update =
  'ClauseConfig 'False '[ 'SpineTable] (RootField tag) "update" 'Nothing

data Set

type instance ClauseConfigFor tag Set =
  'ClauseConfig 'False '[ 'SpineTable, 'SpineQuery] [CondField tag] "set" 'Nothing

data On

type instance ClauseConfigFor tag On =
  'ClauseConfig 'False '[ 'SpineTable, 'SpineQuery] [CondField tag] "on" 'Nothing

data Where

type instance ClauseConfigFor tag Where =
  'ClauseConfig 'False '[ 'SpineQuery] [CondField tag] "where" 'Nothing

data Values

type instance ClauseConfigFor tag Values =
  'ClauseConfig 'False '[ 'SpineQuery, 'SpineTable] [Field tag] "values" 'Nothing

data OnConflict

type instance ClauseConfigFor tag OnConflict =
  'ClauseConfig 'False '[ 'SpineQuery, 'SpineTable] [Field tag] "on conflict" 'Nothing

data DoUpdateSet

type instance ClauseConfigFor tag DoUpdateSet =
  'ClauseConfig 'False '[ 'SpineQuery, 'SpineTable] [CondField tag] "do update set" 'Nothing

data Returning

type instance ClauseConfigFor tag Returning =
  'ClauseConfig 'True '[ 'SpineTable] [Field tag] "returning" 'Nothing

data CreateTable

type instance ClauseConfigFor tag CreateTable =
  'ClauseConfig 'False '[ 'SpineTable] (RootField tag) "create table" 'Nothing

data DropTable

type instance ClauseConfigFor tag DropTable =
  'ClauseConfig 'False '[ 'SpineTable] (ClauseParam (RootField tag) Drop) "drop table" 'Nothing

data CreateType

type instance ClauseConfigFor tag CreateType =
  'ClauseConfig 'False '[ 'SpineTable] (TypeField tag) "create type" 'Nothing

data OrderBy

type instance ClauseConfigFor tag OrderBy =
  'ClauseConfig 'False '[ 'SpineTable, 'SpineProj] (ClauseParam (PrimField tag) Order) "order by" 'Nothing

data Limit

type instance ClauseConfigFor tag Limit =
  'ClauseConfig 'False '[ 'SpineQuery] (OrLiteral Int64 (PrimField tag)) "limit" ('Just Int64)

data Offset

type instance ClauseConfigFor tag Offset =
  'ClauseConfig 'False '[ 'SpineQuery] (PrimField tag) "offset" 'Nothing
