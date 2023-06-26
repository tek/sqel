module Sqel.Class.ClauseKeyword where

import Sqel.Data.ClauseConfig (ClauseKeywordFor)
import Sqel.Data.Sql (Sql (Sql))
import Sqel.SOP.Constraint (symbolText)

class ClauseKeyword clause where
  clauseKeyword :: Sql

instance (
    keyword ~ ClauseKeywordFor clause,
    KnownSymbol keyword
  ) => ClauseKeyword clause where
    clauseKeyword = Sql (symbolText @keyword)
