module Sqel.Statement.PgSchema where

import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders

import Sqel.Data.ExistingColumn (ExistingColumn (ExistingColumn))
import Sqel.Data.Sql (Sql, sql)
import Sqel.Data.Statement (Statement (Statement))

dbColumns ::
  Sql ->
  Statement Text ExistingColumn
dbColumns code =
  Statement code encoder decoder
  where
    decoder =
      ExistingColumn <$> text' <*> text' <*> text' <*> Decoders.column (Decoders.nullable Decoders.text) <*> (yesOrNo <$> text')
    text' =
      Decoders.column (Decoders.nonNullable Decoders.text)
    yesOrNo = \case
      "YES" -> True
      _ -> False
    encoder =
      Encoders.param (Encoders.nonNullable Encoders.text)

-- TODO try turning this into Dd
columnsSql :: Sql -> Sql -> Sql -> Sql
columnsSql entity container namePrefix =
  [sql|select c.#{entity}_name, c.data_type, c.#{namePrefix}udt_name, e.data_type, c.is_nullable
       from information_schema.#{entity}s c
       left join information_schema.element_types e
       on (
         (c.#{container}_catalog,
         c.#{container}_schema,
         c.#{container}_name,
         'TABLE',
         c.dtd_identifier
        ) = (
          e.object_catalog,
          e.object_schema,
          e.object_name,
          e.object_type,
          e.collection_type_identifier
        )
      )
       where c.#{container}_name = $1|]

tableColumnsSql :: Sql
tableColumnsSql =
  columnsSql "column" "table" ""

typeColumnsSql :: Sql
typeColumnsSql =
  columnsSql "attribute" "udt" "attribute_"
