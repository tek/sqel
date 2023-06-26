module Sqel.Test.SqelTypesTest where

import GHC.Records (getField)
import Generics.SOP (NP (Nil, (:*)))
import Hedgehog (TestT, (===))

import Sqel.Class.ReifySqel (sqel)
import Sqel.Class.TableTypes (SqelTableTypes (sqelTableTypes))
import Sqel.Data.Sqel (Projected)
import Sqel.Default (Sqel)
import Sqel.Dsl

data Quality =
  Quality {
    consistency :: Double,
    contiguity :: Double
  }
  deriving stock (Eq, Show, Generic)

data Prop =
  Good { magnitude :: Double, perfect :: Bool }
  |
  Unclear { quality :: Quality }
  deriving stock (Eq, Show, Generic)

data Tab =
  Tab {
    name :: Text,
    number :: Int,
    prop :: Prop
  }
  deriving stock (Eq, Show, Generic)

type Table_Tab =
  Table "tab" Tab (Prod [
    Prim,
    Prim,
    Sum [Con [Prim, Prim], Con1 (Prod [Prim, Prim])]
  ])

type Type_Prop = Projected "prop" Table_Tab
type Type_Good = Projected "Good" Type_Prop
type Type_Quality = Projected "quality" (Projected "Unclear" Type_Prop)

table_Tab :: Sqel Table_Tab
table_Tab = sqel

type_Prop :: Sqel Type_Prop
type_Prop = table_Tab.prop

type_Good :: Sqel Type_Good
type_Good = (getField @"Good" type_Prop)

type_Quality :: Sqel Type_Quality
type_Quality = (getField @"Unclear" type_Prop).quality

target :: NP Sqel [Type_Prop, Type_Good, Type_Quality]
target =
  type_Prop :*
  type_Good :*
  type_Quality :*
  Nil

test_sqelTypes :: TestT IO ()
test_sqelTypes =
  target === sqelTableTypes table_Tab
