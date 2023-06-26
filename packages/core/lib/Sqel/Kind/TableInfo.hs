module Sqel.Kind.TableInfo where

import Sqel.Data.Dd (Dd, DdK (Dd))
import Sqel.Kind.CheckPrim (MkTableFPaths, TableFPaths)

data TableInfo =
  IsTable Symbol
  |
  AvailTables [TableFPaths]

type IsTable :: Type -> [Dd] -> Bool
type family IsTable a tables where
  IsTable _ '[] = 'False
  IsTable a ('Dd _ a _ : _) = 'True
  IsTable a (_ : tables) = IsTable a tables

type MkTableInfo' :: Bool -> Symbol -> [Dd] -> TableInfo
type family MkTableInfo' isTable name tables where
  MkTableInfo' 'True name _ = 'IsTable name
  MkTableInfo' 'False _ tables = 'AvailTables (MkTableFPaths tables)

type MkTableInfo :: Type -> Symbol -> [Dd] -> TableInfo
type family MkTableInfo a name tables where
  MkTableInfo a name tables = MkTableInfo' (IsTable a tables) name tables
