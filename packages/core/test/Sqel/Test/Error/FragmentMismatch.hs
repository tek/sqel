{-# options_ghc -fdefer-type-errors -Wno-deferred-type-errors #-}

module Sqel.Test.Error.FragmentMismatch where

import Prelude hiding (Mod, join, on)

import Sqel.Clauses (from, join, on, select, where_)
import Sqel.Data.Sql (Sql)
import Sqel.Fragment ((.=))
import Sqel.Syntax.Fragments (queryK)
import qualified Sqel.Syntax.Monad as S
import Sqel.Test.Statement.Common (Query_Q, Table_Bird, Table_Cat)
import Sqel.Default (Def)

tableForQuery :: Sql
tableForQuery =
  S.do
    c <- queryK @Query_Q @[Table_Cat, Table_Bird] @Def
    select c.bird.num
    from c.cat
    join c.bird
    on (c.cat.nam .= c.bird.cat)
    where_ c.cat

notAFragment :: Sql
notAFragment =
  S.do
    c <- queryK @Query_Q @[Table_Cat, Table_Bird] @Def
    select c.bird.num
    from c.cat
    join c.bird
    on (c.cat.nam .= c.bird.cat)
    where_ c

notRoot :: Sql
notRoot =
  S.do
    c <- queryK @Query_Q @[Table_Cat, Table_Bird] @Def
    select c.bird.num
    from c.cat.nam
