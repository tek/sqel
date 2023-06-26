module Sqel.Data.Crud where

import qualified Hasql.Statement as Hasql

import Sqel.Data.Statement (Statement)

data Crud q d =
  Crud {
    insert :: Statement d (),
    upsert :: Statement d (),
    delete :: Statement q d,
    deleteAll :: Statement () d,
    fetch :: Statement q d,
    fetchAll :: Statement () d
  }
  deriving stock (Generic)

data CrudH f q d =
  CrudH {
    insert :: Hasql.Statement d (),
    upsert :: Hasql.Statement d (),
    delete :: Hasql.Statement q (f d),
    deleteAll :: Hasql.Statement () [d],
    fetch :: Hasql.Statement q (f d),
    fetchAll :: Hasql.Statement () [d]
  }
  deriving stock (Generic)
