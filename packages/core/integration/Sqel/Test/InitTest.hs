module Sqel.Test.InitTest where

import Hasql.Session (Session)
import Hedgehog (TestT, (===))

import Sqel.Class.ReifySqel (sqel)
import Sqel.Data.Uid (Uid (Uid))
import Sqel.Default (Sqel)
import Sqel.Dsl (Gen, IntTable, Prim, Prod)
import Sqel.Migration.Init (initTable)
import Sqel.Statement (runUnprepared)
import qualified Sqel.Statement.Common as Statement
import Sqel.Test.Run (integrationTest, stmt_)

data Pord =
  Pord {
    name :: Text,
    number :: Int
  }
  deriving stock (Eq, Show, Generic)

data Dat =
  Dat {
    int :: Int,
    pord :: Pord
  }
  deriving stock (Eq, Show, Generic)

type Table_Dat = IntTable "dat" Dat (Prod [Prim, Gen])

table_Dat :: Sqel Table_Dat
table_Dat = sqel

session :: Session [Uid Int64 Dat]
session = do
  stmt_ "drop table if exists dat"
  stmt_ "drop type if exists sqel_type__pord"
  initTable table_Dat
  runUnprepared @() (Uid 1 (Dat 1 (Pord "1" 1))) (Statement.insert table_Dat)
  runUnprepared () (Statement.selectAll table_Dat)

test_init :: TestT IO ()
test_init =
  integrationTest \ exec -> do
    r <- exec session
    [Uid 1 (Dat 1 (Pord "1" 1))] === r
