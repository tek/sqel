module Sqel.Dsl.Fields where

import Generics.SOP.Type.Metadata (FieldInfo (FieldInfo))

import Sqel.Kind.List (type (++))
import Sqel.Kind.Nat (NatSymbol)

data Field = FieldNamed Symbol | FieldNum Nat

type NamedFields :: [FieldInfo] -> [Field]
type family NamedFields fields where
  NamedFields '[] = '[]
  NamedFields ('FieldInfo n : fields) = 'FieldNamed n : NamedFields fields

type ReifyFieldNamesMulti :: Symbol -> [Field] -> [Symbol]
type family ReifyFieldNamesMulti con fields where
  ReifyFieldNamesMulti _ '[] = '[]
  ReifyFieldNamesMulti con ('FieldNum num : fields) = (con ++ NatSymbol num) : ReifyFieldNames con fields
  ReifyFieldNamesMulti con ('FieldNamed name : fields) = name : ReifyFieldNames con fields

type ReifyFieldNames :: Symbol -> [Field] -> [Symbol]
type family ReifyFieldNames con fields where
  ReifyFieldNames con '[ 'FieldNum _ ] = '[con]
  ReifyFieldNames con fields = ReifyFieldNamesMulti con fields
