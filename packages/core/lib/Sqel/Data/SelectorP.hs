module Sqel.Data.SelectorP where

import Sqel.Data.PgTypeName (PgTableName)
import Sqel.Data.Selector (Selector (Selector), pathSelectorTable)
import Sqel.Data.Sql (Sql)

-- TODO this can be subsumed by PrimPath
data SelectorP =
  SelectorP {
    table :: Maybe PgTableName,
    root :: Text,
    sub :: [Text]
  }
  deriving stock (Eq, Show, Generic)

formatSelectorP :: SelectorP -> Sql
formatSelectorP SelectorP {..} =
  coerce (pathSelectorTable table (root :| sub))
