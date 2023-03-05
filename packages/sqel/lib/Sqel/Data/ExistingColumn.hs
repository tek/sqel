{-# language NoFieldSelectors #-}

module Sqel.Data.ExistingColumn where

data ExistingColumn =
  ExistingColumn {
    name :: Text,
    dataType :: Text,
    udtName :: Text,
    elementDataType :: Maybe Text,
    isNullable :: Bool
  }
  deriving stock (Eq, Show, Generic)
