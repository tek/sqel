module Sqel.Class.ResultShape where

import qualified Data.Set as Set
import Data.Tuple (Solo (Solo))
import Data.Vector (Vector)
import Hasql.Decoders (Result, Row, noResult, rowList, rowMaybe, rowVector, rowsAffected, singleRow)

newtype RowsAffected a =
  RowsAffected Int64
  deriving stock (Eq, Show, Generic)

class ResultShape a result | result -> a where
  resultShape :: Row a -> Result result

instance ResultShape a [a] where
  resultShape = rowList

instance ResultShape a (Vector a) where
  resultShape = rowVector

instance Ord a => ResultShape a (Set a) where
  resultShape = fmap Set.fromList . rowList

instance ResultShape a (Maybe a) where
  resultShape = rowMaybe

instance ResultShape () () where
  resultShape = const noResult

instance ResultShape a (Identity a) where
  resultShape = fmap Identity . singleRow

instance ResultShape a (Solo a) where
  resultShape = fmap Solo . singleRow

instance ResultShape a (RowsAffected a) where
  resultShape _ = RowsAffected <$> rowsAffected
