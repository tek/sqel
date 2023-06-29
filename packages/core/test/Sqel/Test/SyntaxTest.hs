module Sqel.Test.SyntaxTest where

import Exon (exon)
import Exon.SkipWs (intron)
import Hedgehog (TestT, (===))
import Prelude hiding (join, on)

import Sqel.Build.Sql (buildSqlDd)
import Sqel.Clauses (from, join, on, select, where_)
import Sqel.Data.Sql (Sql)
import Sqel.Data.Statement (Statement, statementSql)
import Sqel.Default (Def)
import Sqel.Fragment ((.=))
import Sqel.Syntax.Fragments (query1, query1K, queryK)
import qualified Sqel.Syntax.Monad as S
import Sqel.Test.Statement.Common (Fur, Q, Query_Q, Table_Bird, Table_Cat, query_Q, table_Cat)

stmt1 :: Sql
stmt1 =
  buildSqlDd @('Just Query_Q) @'[Table_Cat] @Def \ c -> S.do
    select (c.cat.fur.color, c.cat.fur.density)
    from c.cat
    where_ c.query.fur

stmt2 :: Sql
stmt2 =
  statementSql @Q @Fur S.do
    c <- query1 query_Q table_Cat
    select (c.cat.fur.color, c.cat.fur.density)
    from c.cat
    where_ c.query.fur

stmt14 :: Statement Q Fur
stmt14 = S.do
  c <- query1K @Query_Q @Table_Cat @Def
  select (c.cat.fur.color, c.cat.fur.density)
  from c.cat
  where_ c.query.fur

stmt3 :: Sql
stmt3 =
  statementSql S.do
    c <- queryK @Query_Q @'[Table_Cat, Table_Bird] @Def
    select c.cat.fur
    from c.cat
    join c.bird
    on (c.cat.nam .= c.bird.cat)
    where_ c.query

target1 :: Sql
target1 = [exon|select ("fur")."color", ("fur")."density" from "cat" where ("fur")."color" = $2|]

target2 :: Sql
target2 = [intron|
  select ("cat"."fur")."color", ("cat"."fur")."density"
  from "cat" cat
  join "bird" bird
  on "cat"."nam" = "bird"."cat"
  where "cat"."nam" = $1
  and ("cat"."fur")."color" = $2
  |]

test_syntax :: TestT IO ()
test_syntax = do
  target1 === stmt1
  target1 === stmt2
  target2 === stmt3
