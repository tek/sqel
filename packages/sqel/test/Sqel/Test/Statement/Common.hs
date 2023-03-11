module Sqel.Test.Statement.Common where

import Sqel.Data.Dd (Dd)
import Sqel.Prim (prims)
import Sqel.Product (prod)
import Sqel.Type (Prim, Prod, type (*>), type (>))

data Pro =
  Pro {
    num :: Int64,
    name :: Text
  }
  deriving stock (Eq, Show, Generic)

type ProdTable =
  Prod Pro *> (Prim "num" Int64 > Prim "name" Text)

ddPro :: Dd ProdTable
ddPro =
  prod prims
