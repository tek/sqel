{-# language QualifiedDo #-}

module Sqel.Crud (
  module Sqel.Data.Crud,
  module Sqel.Crud,
) where

import Sqel.Class.Check (Check1)
import Sqel.Class.ResultShape (ResultShape)
import Sqel.Data.Crud (Crud (..), CrudH (..))
import Sqel.Dd (DdType)
import Sqel.Default (Sqel)
import qualified Sqel.Statement as Statement

crud ::
  ∀ q d ds qs .
  q ~ DdType qs =>
  d ~ DdType ds =>
  Check1 ds qs =>
  Sqel qs ->
  Sqel ds ->
  Crud q d
crud query table =
  Crud {
    insert = Statement.insert table,
    upsert = Statement.upsert table,
    delete = Statement.delete query table,
    deleteAll = Statement.deleteAll table,
    fetch = Statement.selectWhere query table,
    fetchAll = Statement.selectAll table,
    ..
  }

prepared ::
  ∀ f q d .
  ResultShape d (f d) =>
  Crud q d ->
  CrudH f q d
prepared Crud {..} =
  CrudH {
    insert = Statement.prepared insert,
    upsert = Statement.prepared upsert,
    delete = Statement.prepared delete,
    deleteAll = Statement.prepared deleteAll,
    fetch = Statement.prepared fetch,
    fetchAll = Statement.prepared fetchAll
  }
