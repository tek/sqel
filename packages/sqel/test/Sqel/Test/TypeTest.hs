module Sqel.Test.TypeTest where

import Data.Type.Equality ((:~:) (Refl))
import Hedgehog (TestT)
import Prelude hiding (sum)

import Sqel.Data.Dd (Dd)
import Sqel.Prim (primNewtypes)
import Sqel.Product (prod)
import Sqel.Type (PrimNewtype, Prod, ProdPrimsNewtype, type (*>), type (>))

newtype IntNt =
  IntNt { unIntNt :: Int }
  deriving stock (Eq, Show, Generic)

data Three =
  Three {
    a :: IntNt,
    b :: IntNt,
    c :: IntNt
  }
  deriving stock (Eq, Show, Generic)

type ThreeTableGen =
  ProdPrimsNewtype Three

type ThreeTable =
  Prod Three *> PrimNewtype "a" IntNt > PrimNewtype "b" IntNt > PrimNewtype "c" IntNt

ddThree :: Dd ThreeTableGen
ddThree =
  prod primNewtypes

test_prodGen :: TestT IO ()
test_prodGen =
  case Refl :: ThreeTable :~: ThreeTableGen of
    Refl -> unit
