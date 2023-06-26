module Sqel.Class.HasqlStatement where

import Hasql.Decoders (Row)
import Hasql.Encoders (Params)
import qualified Hasql.Statement as Hasql

import Sqel.Data.Sql (Sql (Sql))
import qualified Sqel.Data.Statement
import Sqel.Data.Statement (Statement (Statement))
import Sqel.Class.ResultShape (ResultShape (resultShape))

toHasql ::
  ResultShape d result =>
  Bool ->
  Sql ->
  Params p ->
  Row d ->
  Hasql.Statement p result
toHasql prep (Sql s) params row =
  Hasql.Statement (encodeUtf8 s) params (resultShape row) prep

type HasqlStatement :: Type -> Type -> Constraint
class HasqlStatement a result | result -> a where
  hasqlStatement :: Bool -> Statement query a -> Hasql.Statement query result

instance result ~ () => HasqlStatement () result where
  hasqlStatement prep Statement {..} = toHasql prep sql encoder decoder

instance {-# incoherent #-} (
    ResultShape a result
  ) => HasqlStatement a result where
    hasqlStatement prep Statement {..} = toHasql prep sql encoder decoder
