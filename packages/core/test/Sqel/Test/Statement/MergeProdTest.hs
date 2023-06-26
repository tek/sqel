module Sqel.Test.Statement.MergeProdTest where

import Hedgehog (TestT, (===))

import Sqel.Clauses (from, select)
import Sqel.Data.Sql (Sql, sql)
import Sqel.Default (Def)
import Sqel.Dsl (Gen, Prim, Prod, Table)
import Sqel.Syntax.Fragments (tableK)
import qualified Sqel.Syntax.Monad as S
import Sqel.Test.Statement.Common (Fur)

data MergeProd =
  MergeProd { count :: Int, fur :: Fur }
  deriving stock (Eq, Show, Generic)

type Table_MergeProd =
  Table "merge_prod" MergeProd (Prod [Prim, Gen])

target_merge_prod :: Sql
target_merge_prod =
  [sql|select "count", ("fur")."color", ("fur")."density" from "merge_prod"|]

test_statement_merge_prod :: TestT IO ()
test_statement_merge_prod =
  target_merge_prod === S.do
    t <- tableK @Table_MergeProd @Def
    select t
    from t
