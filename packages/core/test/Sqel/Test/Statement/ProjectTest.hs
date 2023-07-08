module Sqel.Test.Statement.ProjectTest where

import Exon (exon)
import Hedgehog (TestT, (===))

import Sqel.Class.Check (Check1)
import Sqel.Class.NamedFragment (NamedProjection)
import Sqel.Class.ReifySqel (sqel)
import Sqel.Clauses (from, select, where_)
import Sqel.Data.Sql (Sql)
import qualified Sqel.Data.Statement as Statement
import Sqel.Data.Statement (Statement)
import Sqel.Data.TestTables (query_Q, table_Cat)
import Sqel.Data.Uid (Uid)
import Sqel.Dd (DdType)
import Sqel.Default (Sqel)
import Sqel.Dsl
import Sqel.Syntax.Fragments (project, query1)
import qualified Sqel.Syntax.Monad as S

-- TODO when using the wrong result type, the error doesn't show @ProjectionNamed@, but @HFindT@.
-- Would be good to have a custom error here, unclear whether it's feasible.
-- TODO without Check present, it complains about kind mismatch for @ext@
statement ::
  ∀ query table proj p .
  Check1 table query =>
  Check1 table proj =>
  NamedProjection "fur" '[proj] p =>
  Sqel query ->
  Sqel table ->
  Sqel proj ->
  Statement '[DdType table] (DdType query) (DdType p)
statement query table proj = S.do
  frags <- project proj (query1 query table)
  select frags.projections.fur
  from frags.table
  where_ frags.query

target1 :: Sql
target1 = [exon|select ("fur")."color", ("fur")."density" from "cat" where "nam" = $1 and ("fur")."color" = $2|]

test_project :: TestT IO ()
test_project = do
  target1 === (statement query_Q table_Cat table_Cat.fur).sql

data PM2 =
  PM2 {
    num :: Int64
  }
  deriving stock (Eq, Show, Generic)

type Proj_PM2 = IntTable "pm2" PM2 (Path '["pm2"] (Prod '[Prim]))

proj_PM2 :: Sqel Proj_PM2
proj_PM2 = sqel

data PM1 =
  PM1 {
    pm2 :: PM2
  }
  deriving stock (Eq, Show, Generic)

type Proj_PM1 = IntTable "pm1" PM1 (Prod '[Prod '[Prim]])

proj_PM1 :: Sqel Proj_PM1
proj_PM1 = sqel

data PM =
  PM {
    pm1 :: PM1
  }
  deriving stock (Eq, Show, Generic)

type Table_PM = IntTable "pm" PM (Prod '[Merge (Prod '[Prod '[Prim]])])

table_PM :: Sqel Table_PM
table_PM = sqel

data Q2 =
  Q2 {
    num :: Int64
  }
  deriving stock (Eq, Show, Generic)

type Query_Q2 = Query Q2 (Path '["pm2"] (Name "pm2" (Prod '[Prim])))

query_Q2 :: Sqel Query_Q2
query_Q2 = sqel

statement2 ::
  Check1 table Query_Q2 =>
  Check1 table Proj_PM2 =>
  Sqel table ->
  Statement '[DdType table] Q2 (Uid Int64 PM2)
statement2 table = S.do
  frags <- project proj_PM2 (query1 query_Q2 table)
  select frags.projection
  from frags.table
  where_ frags.query

target2 :: Sql
target2 = [exon|select "id", ("pm2")."num" from "pm" where ("pm2")."num" = $1|]

test_project_merge :: TestT IO ()
test_project_merge =
  target2 === (statement2 table_PM).sql
