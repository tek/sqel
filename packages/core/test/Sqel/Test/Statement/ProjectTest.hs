module Sqel.Test.Statement.ProjectTest where

import Exon (exon)
import Hedgehog (TestT, (===))

import Sqel.Class.Check (Check1)
import Sqel.Class.NamedFragment (NamedProjection)
import Sqel.Clauses (from, select, where_)
import Sqel.Data.Sql (Sql)
import qualified Sqel.Data.Statement as Statement
import Sqel.Data.Statement (Statement)
import Sqel.Dd (DdType)
import Sqel.Default (Sqel)
import Sqel.Syntax.Fragments (project, query1)
import qualified Sqel.Syntax.Monad as S
import Sqel.Data.TestTables (query_Q, table_Cat)

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
  Statement (DdType query) (DdType p)
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
