module Sqel.Test.SqelTypesTest where

import GHC.Records (getField)
import Generics.SOP (NP (Nil, (:*)))
import Hedgehog (TestT, (===))

import Sqel.Class.ReifySqel (sqel)
import Sqel.Class.TableTypes (SqelTableTypes (sqelTableTypes))
import Sqel.Default (Sqel)
import Sqel.Dsl (Con, Con1, Prim, Prod, Sum, Table)
import Sqel.Kind.Project (type (.))

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

type Type_Prop = Table_Tab . "prop"
type Type_Good = Type_Prop . "Good"
type Type_Quality = Type_Prop . "Unclear" . "quality"

table_Tab :: Sqel Table_Tab
table_Tab = sqel

type_Prop :: Sqel Type_Prop
type_Prop = table_Tab.prop

type_Good :: Sqel Type_Good
type_Good = (getField @"Good" type_Prop)

type_Quality :: Sqel Type_Quality
type_Quality = type_Prop.quality

target :: NP Sqel [Type_Prop, Type_Good, Type_Quality]
target =
  type_Prop :*
  type_Good :*
  type_Quality :*
  Nil

test_sqelTypes :: TestT IO ()
test_sqelTypes =
  target === sqelTableTypes table_Tab
