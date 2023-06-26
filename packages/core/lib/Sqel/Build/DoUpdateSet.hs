module Sqel.Build.DoUpdateSet where

import Exon (exon)

import Sqel.Build.Columns (fieldColumns)
import Sqel.Build.Values (fieldValues)
import Sqel.Data.Field (Field)
import Sqel.Data.Sql (Sql)
import Sqel.Default (Def)
import Sqel.Sql (joinComma)

setter :: Sql -> Sql -> Sql
setter name value =
  [exon|#{name} = #{value}|]

setters :: Field Def -> [Sql]
setters f =
   uncurry setter <$> zip names (foldMap toList (fieldValues f))
  where
    names = fieldColumns f

doUpdateSetClause :: [Field Def] -> Sql
doUpdateSetClause fields =
  joinComma (mconcat (setters <$> fields))
