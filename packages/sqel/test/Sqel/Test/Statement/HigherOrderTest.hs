module Sqel.Test.Statement.HigherOrderTest where

import Generics.SOP (NP (..))
import Hedgehog (TestT, (===))
import Prelude hiding (sum)

import Sqel.Class.MatchView (HasPath)
import Sqel.Comp (Column, CompName (compName))
import Sqel.Data.Codec (FullCodec)
import Sqel.Data.Dd (
  Dd (Dd),
  DdInc (DdNest),
  DdK (DdK),
  DdSort (DdProd),
  DdStruct (DdComp),
  DdType,
  DdTypeSel,
  type (:>) ((:>)),
  )
import Sqel.Data.Mods (pattern NoMods)
import Sqel.Data.QuerySchema (QuerySchema)
import Sqel.Data.Sel (MkTSel (mkTSel), Sel (SelAuto), SelW (SelWAuto))
import Sqel.Data.Sql (Sql, sql, toSql)
import Sqel.Data.SqlFragment (Select (Select))
import Sqel.Data.TableSchema (TableSchema)
import Sqel.Data.Uid (Uid)
import Sqel.Merge (merge)
import Sqel.PgType (MkTableSchema, tableSchema)
import Sqel.Prim (prim, primAs)
import Sqel.Product (prod, prodSel)
import Sqel.Query (checkQuery)
import Sqel.ReifyCodec (ReifyCodec)
import Sqel.ReifyDd (ReifyDd)
import qualified Sqel.Sql.Select as Sql
import Sqel.Sum (con1, sum)
import Sqel.Test.Statement.Common (Pro, ProdTable, ddPro)
import qualified Sqel.Type as T
import Sqel.Type (Prim, Prod, TypeSel, type (*>), type (>))
import Sqel.Uid (UidDd, uid)

data Wrap a =
  Wrap { wrapped :: a, length :: Int64 }
  deriving stock (Eq, Show, Generic)

instance CompName a name => CompName (Wrap a) name where
  compName = compName @a

type WrapDd sa =
  TypeSel (DdTypeSel sa) (Prod (Wrap (DdType sa))) *> (
    T.Merge sa >
    Prim "length" Int64
  )

schema_higherOrder ::
  ∀ s0 table a sel mods s .
  s0 ~ 'DdK sel mods a s =>
  table ~ WrapDd s0 =>
  MkTSel (DdTypeSel s0) =>
  MkTableSchema table =>
  Dd s0 ->
  TableSchema (Wrap a)
schema_higherOrder wrapped =
  tableSchema dd
  where
    dd :: Dd table
    dd = Dd SelWAuto NoMods (DdComp mkTSel DdProd DdNest fields)
    fields = merge wrapped :* primAs @"length" :* Nil

target_higherOrder :: Sql
target_higherOrder =
  [sql|select "num", "name", "length" from "pro"|]

test_statement_higherOrder :: TestT IO ()
test_statement_higherOrder =
  target_higherOrder === toSql (Select (schema_higherOrder ddPro))

data Merge1 =
  One { one :: Pro }
  |
  Two { two :: Pro }
  deriving stock (Eq, Show, Generic)

data QHo = QHo { name :: Text }
  deriving stock (Eq, Show, Generic)

data QWrap = QWrap { two :: QHo }
  deriving stock (Eq, Show, Generic)

statement_query_higherOrder ::
  ∀ a s0 table mods s .
  s0 ~ 'DdK 'SelAuto mods a s =>
  MkTSel (DdTypeSel s0) =>
  table ~ UidDd (Prim "id" Int64) (WrapDd s0) =>
  MkTableSchema table =>
  HasPath ["two", "name"] Text table =>
  Dd s0 ->
  Sql
statement_query_higherOrder wrapped =
  Sql.selectWhere qs ts
  where
    ts :: TableSchema (Uid Int64 (Wrap a))
    ts = tableSchema dd
    qs :: QuerySchema QWrap (Uid Int64 (Wrap a))
    qs = checkQuery q dd
    q = prod (prod prim)
    dd :: Dd table
    dd = uid prim pro
    pro = Dd SelWAuto NoMods (DdComp mkTSel DdProd DdNest fields)
    fields = merge wrapped :* primAs @"length" :* Nil

ddMerge1 :: Dd ('DdK _ _ Merge1 _)
ddMerge1 = sum (con1 ddPro :> con1 ddPro)

target_merge_query_higherOrder :: Sql
target_merge_query_higherOrder =
  [sql|select "id", "sqel_sum_index__merge1", ("one").num, ("one").name, ("two").num, ("two").name, "length"
       from "merge1" where ((("two")."name" = $1))|]

test_statement_merge_query_higherOrder :: TestT IO ()
test_statement_merge_query_higherOrder =
  target_merge_query_higherOrder === statement_query_higherOrder ddMerge1

ddHigherOrder2 ::
  ∀ s merged .
  merged ~ T.Merge s =>
  MkTSel (DdTypeSel s) =>
  Column (DdType s) "wrapped" merged merged =>
  Dd s ->
  Dd (UidDd (Prim "id" Int64) (WrapDd s))
ddHigherOrder2 wrapped =
  uid prim (prodSel @(DdTypeSel s) (merge wrapped :> prim))

ddUidWrapPro :: Dd (UidDd (Prim "id" Int64) (WrapDd ProdTable))
ddUidWrapPro =
  ddHigherOrder2 ddPro

higherOrder2 ::
  ∀ a s merged .
  merged ~ T.Merge s =>
  MkTSel (DdTypeSel s) =>
  Column a "wrapped" merged merged =>
  ReifyDd merged =>
  ReifyCodec FullCodec merged a =>
  Dd s ->
  Sql
higherOrder2 wrapped =
  toSql (Select ts)
  where
    ts :: TableSchema (Wrap a)
    ts = tableSchema dd
    dd = prodSel @(DdTypeSel s) (merge wrapped :> prim)

test_higherOrder2 :: TestT IO ()
test_higherOrder2 =
  [sql|select "num", "name", "length" from "pro"|] === higherOrder2 ddPro
