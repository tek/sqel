module Sqel.Data.Constraints where

import Data.Aeson (FromJSON, ToJSON)
import qualified Exon

import Sqel.Data.Sql (Sql, ToSql (toSql))

data ConstraintsK =
  ConstraintsK {
    unique :: Bool,
    nullable :: Bool,
    primary :: Bool,
    custom :: [Symbol]
  }

type DefaultConstraints =
  'ConstraintsK 'False 'False 'False '[]

data Constraints =
  Constraints {
    unique :: Bool,
    nullable :: Bool,
    primary :: Bool,
    basic :: [Sql]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

renderConstraints :: Constraints -> [Sql]
renderConstraints Constraints {..} =
  add primaryM (add uniqueM (add notNullM basic))
    where
      add = maybe id (:)
      uniqueM | unique, False <- primary = Just "unique"
              | otherwise = Nothing
      notNullM | nullable = Nothing
               | otherwise = Just "not null"
      primaryM | primary = Just "primary key"
               | otherwise = Nothing

instance ToSql Constraints where
  toSql = Exon.intercalate " " . renderConstraints

instance Default Constraints where
  def = Constraints {unique = False, nullable = False, primary = False, basic = []}
