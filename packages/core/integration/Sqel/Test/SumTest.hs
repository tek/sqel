module Sqel.Test.SumTest where

import Hasql.Session (Session)
import Hedgehog (TestT, (===))

import Sqel.Class.ReifySqel (sqel)
import Sqel.Default (Sqel)
import Sqel.Dsl (Gen, Table)
import Sqel.Migration.Init (initTable)
import Sqel.Statement (runUnprepared)
import qualified Sqel.Statement.Common as Statement
import Sqel.Test.Run (integrationTest, stmt_)

data S =
  S1 { s1 :: Int64 }
  |
  S2 { s2 :: Int64 }
  deriving stock (Eq, Show, Generic, Ord)

type Table_S = Table "s" S Gen

table_S :: Sqel Table_S
table_S = sqel

session :: Session (Set S)
session = do
  stmt_ "drop type if exists sqel_type__s1"
  stmt_ "drop type if exists sqel_type__s2"
  stmt_ "drop table if exists s"
  initTable table_S
  runUnprepared (S1 5) (Statement.insert table_S)
  runUnprepared (S2 13) (Statement.insert table_S)
  runUnprepared () (Statement.selectAll table_S)

test_sum :: TestT IO ()
test_sum =
  integrationTest \ exec -> do
    r <- exec session
    [S1 5, S2 13] === r
