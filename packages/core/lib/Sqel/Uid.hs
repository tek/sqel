module Sqel.Uid where

import Data.UUID (UUID)

import Sqel.Class.ReifySqel (ReifySqelFor, sqel)
import Sqel.Data.Sqel (SqelFor)
import Sqel.Dsl (Name, Query)
import Sqel.Dsl.Prim (PrimWith)

type IdQuery i si = Query i (Name "id" si)

type IdQuery_Prim i = Query i (PrimWith "id" i)

type IdQuery_Int = IdQuery_Prim Int64

type IdQuery_UUID = IdQuery_Prim UUID

query_idPrim ::
  ReifySqelFor tag (IdQuery_Prim i) =>
  SqelFor tag (IdQuery_Prim i)
query_idPrim = sqel

query_Int ::
  ReifySqelFor tag IdQuery_Int =>
  SqelFor tag IdQuery_Int
query_Int = sqel

query_UUID ::
  ReifySqelFor tag IdQuery_UUID =>
  SqelFor tag IdQuery_UUID
query_UUID = sqel
