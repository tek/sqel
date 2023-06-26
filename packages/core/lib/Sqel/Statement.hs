module Sqel.Statement (
  module Sqel.Statement,
  module Sqel.Statement.Common,
) where

import Hasql.Decoders (Row)
import Hasql.Encoders (Params)
import qualified Hasql.Session as Session
import Hasql.Session (Session)
import qualified Hasql.Statement as Hasql

import Sqel.Class.HasqlStatement (HasqlStatement (hasqlStatement))
import Sqel.Data.Sql (Sql)
import Sqel.Data.Statement (Statement (Statement))
import Sqel.Statement.Common

prepared ::
  ∀ result query a .
  HasqlStatement a result =>
  Statement query a ->
  Hasql.Statement query result
prepared = hasqlStatement True

unprepared ::
  ∀ result query a .
  HasqlStatement a result =>
  Statement query a ->
  Hasql.Statement query result
unprepared = hasqlStatement False

unsafeSql ::
  ∀ result query a .
  HasqlStatement a result =>
  Sql ->
  Params query ->
  Row a ->
  Hasql.Statement query result
unsafeSql s encoder decoder =
  unprepared (Statement s encoder decoder)

unsafeUntypedSql ::
  Sql ->
  Hasql.Statement () ()
unsafeUntypedSql s =
  unsafeSql s mempty unit

runPrepared ::
  ∀ result query a .
  HasqlStatement a result =>
  query ->
  Statement query a ->
  Session result
runPrepared query =
  Session.statement query . prepared

runUnprepared ::
  ∀ result query a .
  HasqlStatement a result =>
  query ->
  Statement query a ->
  Session result
runUnprepared query =
  Session.statement query . unprepared

unsafeRunSql ::
  ∀ result query a .
  HasqlStatement a result =>
  query ->
  Sql ->
  Params query ->
  Row a ->
  Session result
unsafeRunSql q s encoder decoder =
  runUnprepared q (Statement s encoder decoder)

unsafeRunUntypedSql ::
  Sql ->
  Session ()
unsafeRunUntypedSql s =
  unsafeRunSql () s mempty unit
