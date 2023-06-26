module Sqel.Data.Mods.MigrationRenameIndex where

import Sqel.Class.ReifyDecoder (ReifyDecoder (reifyDecoder))
import Sqel.Class.ReifyEncoder (ReifyEncoder (reifyEncoder))

type MigrationRenameIndex :: Symbol -> Type
data MigrationRenameIndex name =
  MigrationRenameIndex
  deriving stock (Eq, Show, Generic)

type MigrationRenameIndexK :: [Type] -> Maybe Symbol
type family MigrationRenameIndexK mods where
  MigrationRenameIndexK '[] = 'Nothing
  MigrationRenameIndexK (MigrationRenameIndex name : _) = 'Just name
  MigrationRenameIndexK (_ : mods) = MigrationRenameIndexK mods

instance ReifyDecoder mods a => ReifyDecoder (MigrationRenameIndex name : mods) a where
  reifyDecoder = reifyDecoder @mods

instance ReifyEncoder mods a => ReifyEncoder (MigrationRenameIndex name : mods) a where
  reifyEncoder = reifyEncoder @mods
