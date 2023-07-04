module Sqel.Default.Clauses where

import Sqel.Data.Clause (ClauseParam)
import Sqel.Data.ClauseConfig (ClauseConfig (ClauseConfig), ClauseConfigFor)
import Sqel.Data.Drop (Drop)
import Sqel.Data.Field (CondField, Field, PrimField, RootField, TypeField)
import Sqel.Data.Order (Order)
import Sqel.Data.Spine (SpineSort (SpineProj, SpineQuery, SpineTable))

data Select

type instance ClauseConfigFor tag Select =
  'ClauseConfig 'True '[ 'SpineTable, 'SpineProj] [Field tag] "select"

data DeleteFrom

type instance ClauseConfigFor tag DeleteFrom =
  'ClauseConfig 'False '[ 'SpineTable] (RootField tag) "delete from"

data From

type instance ClauseConfigFor tag From =
  'ClauseConfig 'False '[ 'SpineTable] (RootField tag) "from"

data Join

type instance ClauseConfigFor tag Join =
  'ClauseConfig 'False '[ 'SpineTable] (RootField tag) "join"

data InsertInto

type instance ClauseConfigFor tag InsertInto =
  'ClauseConfig 'False '[ 'SpineTable] (RootField tag) "insert into"

data On

type instance ClauseConfigFor tag On =
  'ClauseConfig 'False '[ 'SpineTable, 'SpineQuery] [CondField tag] "on"

data Where

type instance ClauseConfigFor tag Where =
  'ClauseConfig 'False '[ 'SpineQuery] [CondField tag] "where"

data Values

type instance ClauseConfigFor tag Values =
  'ClauseConfig 'False '[ 'SpineQuery, 'SpineTable] [Field tag] "values"

data OnConflict

type instance ClauseConfigFor tag OnConflict =
  'ClauseConfig 'False '[ 'SpineQuery, 'SpineTable] [Field tag] "on conflict"

data DoUpdateSet

type instance ClauseConfigFor tag DoUpdateSet =
  'ClauseConfig 'False '[ 'SpineQuery, 'SpineTable] [Field tag] "do update set"

data Returning

type instance ClauseConfigFor tag Returning =
  'ClauseConfig 'True '[ 'SpineTable] [Field tag] "returning"

data CreateTable

type instance ClauseConfigFor tag CreateTable =
  'ClauseConfig 'False '[ 'SpineTable] (RootField tag) "create table"

data DropTable

type instance ClauseConfigFor tag DropTable =
  'ClauseConfig 'False '[ 'SpineTable] (ClauseParam (RootField tag) Drop) "drop table"

data CreateType

type instance ClauseConfigFor tag CreateType =
  'ClauseConfig 'False '[ 'SpineTable] (TypeField tag) "create type"

data OrderBy

type instance ClauseConfigFor tag OrderBy =
  'ClauseConfig 'False '[ 'SpineTable, 'SpineProj] (ClauseParam (PrimField tag) Order) "order by"

data Limit

type instance ClauseConfigFor tag Limit =
  'ClauseConfig 'False '[ 'SpineQuery] (PrimField tag) "limit"

data Offset

type instance ClauseConfigFor tag Offset =
  'ClauseConfig 'False '[ 'SpineQuery] (PrimField tag) "offset"
