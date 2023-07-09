module Sqel.Test.Migration.ColumnMapTest where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Hedgehog (TestT, (===))

import Sqel.Class.ReifySqel (sqel)
import Sqel.Data.PgType (PgColumnName)
import Sqel.Data.Sqel (sqelSpine)
import Sqel.Data.TestTables (Simp)
import Sqel.Default (Sqel)
import Sqel.Dsl (Con1, Gen, IntTable, Sum)
import Sqel.Migration.Metadata (columnMap)
import Sqel.SOP.Newtype (UnwrapNewtype (unwrapNewtype))
import Sqel.Spine (spineTypeCols)

data S =
  S1 { s1 :: Simp }
  |
  S2 { s2 :: Simp }
  deriving stock (Eq, Show, Generic)

type Table_S = IntTable "s" S (Sum [Con1 Gen, Con1 Gen])

table_S :: Sqel Table_S
table_S = sqel

target :: Set PgColumnName
target = ["id", "sqel_sum_index__s", "s1", "s2"]

test_columnMap :: TestT IO ()
test_columnMap =
  target === (Set.fromList (fst <$> Map.toList (unwrapNewtype (columnMap (spineTypeCols (sqelSpine table_S))))))
