module Sqel.Sql.Prepared where

import Sqel.Data.Sql (Sql, sql)
import Sqel.SOP.Constraint (natInt)

dollar :: Int -> Sql
dollar i =
  [sql|$#{show i}|]

natDollar ::
  ∀ n .
  KnownNat n =>
  Sql
natDollar =
  dollar (natInt @n)
