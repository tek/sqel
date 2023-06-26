module Sqel.Data.Name where

data NamePrefix =
  DefaultPrefix
  |
  NoPrefix
  |
  NamePrefix Symbol

type IndexPrefixed :: NamePrefix -> Symbol -> Symbol
type family IndexPrefixed spec name where
  IndexPrefixed 'DefaultPrefix name = AppendSymbol "sqel_sum_index__" name
  IndexPrefixed 'NoPrefix name = name
  IndexPrefixed ('NamePrefix spec) name = AppendSymbol spec name

data Name =
  NameAuto
  |
  Name Symbol

type ShowName :: Name -> Symbol
type family ShowName name where
  ShowName 'NameAuto = "<auto>"
  ShowName ('Name name) = name

type SetName :: Symbol -> Name -> Name
type family SetName new name where
  SetName name 'NameAuto = 'Name name
  SetName name ('Name _) = 'Name name

type AmendName :: Symbol -> Name -> Name
type family AmendName name sel where
  AmendName name 'NameAuto = 'Name name
  AmendName _ s = s
