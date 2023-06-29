module Sqel.Test.Statement.OrderTest where

import Hedgehog (TestT, (===))

import Sqel.Clauses (from, orderBy, select)
import Sqel.Data.Order (Order (Desc))
import Sqel.Data.Sql (Sql, sql)
import Sqel.Data.Statement (statementSql)
import Sqel.Default (Def)
import Sqel.Dsl
import Sqel.Syntax.Fragments (tableK)
import qualified Sqel.Syntax.Monad as S

data Dat =
  Dat {
    num :: Int
  }
  deriving stock (Eq, Show, Generic)

type Table_Dat = Table "dat" Dat Gen

target_order :: Sql
target_order =
  [sql|select "num" from "dat" order by "num" desc|]

test_statement_order :: TestT IO ()
test_statement_order =
  target_order === statementSql S.do
    t <- tableK @Table_Dat @Def
    select t
    from t
    orderBy t.num Desc
