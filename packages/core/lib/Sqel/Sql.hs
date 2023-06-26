module Sqel.Sql (
  module Sqel.Data.Sql,
  module Sqel.Data.CommaSep,
  module Sqel.Sql.Prepared,
  module Sqel.Sql,
) where

import qualified Exon

import Sqel.Data.CommaSep
import Sqel.Data.Sql
import Sqel.Sql.Prepared

joinComma ::
  Foldable t =>
  t Sql ->
  Sql
joinComma =
  Exon.intercalate ", "

concatMapComma ::
  ∀ t a .
  Monad t =>
  Foldable t =>
  (a -> t Sql) ->
  t a ->
  Sql
concatMapComma f as =
  joinComma (f =<< as)
