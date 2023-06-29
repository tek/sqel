module Sqel.Default where

import Data.Aeson (FromJSON, ToJSON)
import Data.Some (Some)

import Sqel.Data.Clause (ClauseParam)
import Sqel.Data.ClauseConfig (ClauseConfig (ClauseConfig), ClauseConfigFor)
import Sqel.Data.Constraints (Constraints)
import Sqel.Data.Dd (Dd)
import Sqel.Data.Drop (Drop)
import Sqel.Data.Field (CondField, Field, PrimField, RootField, TypeField)
import Sqel.Data.Order (Order)
import Sqel.Data.PgType (PgColumnName, PgPrimName, PgTypeRef)
import Sqel.Data.PgTypeName (PgTableName, PgTypeName)
import Sqel.Data.Spine (CompFor, PrimFor, Spine, SpinePath, SpineSort (SpineProj, SpineQuery, SpineTable))
import Sqel.Data.Sqel (SqelFor)
import Sqel.Data.Sql (Sql)
import Sqel.Error.Clause (ClauseDesc, ClauseError)
import Sqel.Error.Fragment (FragmentMismatch, FragmentMismatchDefault)

data Def = Def
  deriving stock (Show)

data CondMeta =
  CondMeta {
    op :: Sql,
    nullable :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data QueryMeta =
  QuerySynthetic
  |
  QueryMeta {
    index :: Int,
    cond :: Maybe CondMeta
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data PrimMeta =
  PrimMeta {
    name :: PgColumnName,
    path :: SpinePath,
    colType :: PgPrimName,
    table :: PgTableName,
    constr :: Constraints,
    query :: QueryMeta
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

type instance PrimFor Def = PrimMeta

data CompMeta =
  CompMeta {
    name :: PgColumnName,
    typeName :: Some PgTypeName,
    colType :: PgTypeRef,
    table :: PgTableName,
    constr :: Constraints
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

type instance CompFor Def = CompMeta

type SpineDef = Spine Def

type Sqel :: Dd -> Type
type Sqel = SqelFor Def

-- TODO remove, this has no advantage really
-- Or think of some way that the fragment kind can be used
type instance ClauseError Def clause _ =
  "These fragments are incompatible with a " <> ClauseDesc clause <> "."

type instance FragmentMismatch Def clause sort =
  FragmentMismatchDefault clause sort

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
