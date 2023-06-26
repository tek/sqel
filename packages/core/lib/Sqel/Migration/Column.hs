module Sqel.Migration.Column where

import Generics.SOP (NP (Nil, (:*)))

import Sqel.Data.Dd (DdK, Inc (Merge, Nest))
import Sqel.Data.Migration (ColumnAction (AddColumn, RemoveColumn, RenameColumn))
import Sqel.Data.Name (NamePrefix)
import Sqel.Data.PgType (PgColumnName, pgColumnName)
import Sqel.Data.Sel (PrefixedIndexMaybe)
import Sqel.Data.Sqel (SqelFor, sqelSub)
import Sqel.Dd (DdSub)
import Sqel.Kind.List (type (++))
import Sqel.Migration.Ddl (DdlConf (DdlConf), DdlStruct (DdlComp, DdlPrim), GetDdlMods)
import Sqel.SOP.Constraint (symbolText)

data IxDdl =
  IxDdl {
    index :: Nat,
    ddl :: DdlConf
  }

type MkIxDdl :: Nat -> [DdlConf] -> [IxDdl]
type family MkIxDdl index s where
  MkIxDdl _ '[] = '[]
  MkIxDdl index (s : ss) = 'IxDdl index s : MkIxDdl (index + 1) ss

data ModK =
  KeepK
  |
  AddK { name :: Symbol, def :: Maybe Symbol, comp :: Maybe Symbol }
  |
  RenameK { old :: Symbol, new :: Symbol }

data ActionK =
  ActionK {
    mod :: ModK,
    index :: Nat
  }
  |
  RemoveK { name :: Symbol }
  |
  MergeK { index :: Nat, indexAction :: ModK }

-- TODO what if index is Just/Nothing? sum was changed to a product?
type IndexAction :: Symbol -> Symbol -> Maybe NamePrefix -> Maybe NamePrefix -> ModK
type family IndexAction oldName newName old new where
  IndexAction name name prefix prefix =
    'KeepK
  IndexAction oldName newName prefixOld prefixNew =
    'RenameK (PrefixedIndexMaybe prefixOld oldName) (PrefixedIndexMaybe prefixNew newName)

type IndexActionDd :: DdlConf -> DdlConf -> ModK
type family IndexActionDd old new where
  IndexActionDd ('DdlConf _ _ _ _ ('DdlComp _ oldName _ _ indexPrefixOld _)) ('DdlConf _ _ _ _ ('DdlComp _ newName _ _ indexPrefixNew _)) =
    IndexAction oldName newName indexPrefixOld indexPrefixNew
  IndexActionDd _ _ =
    'KeepK

-- TODO could enforce column constraint match
-- TODO Merge/Nest mismatch?
type MigComp :: DdlConf -> [IxDdl] -> [IxDdl] -> (ActionK, [IxDdl])
type family MigComp conf check other where

  MigComp ('DdlConf name _ _ _ ('DdlComp _ oldTName compName _ oldIndex 'Merge))
          ('IxDdl index ('DdlConf name 'Nothing _ _ ('DdlComp _ newTName compName 'Nothing newIndex 'Merge)) : check) other =
    '( 'MergeK index (IndexAction oldTName newTName oldIndex newIndex), check ++ other)

  MigComp ('DdlConf name _ _ _ ('DdlComp _ oldTName compName _ oldIndex 'Merge))
          ('IxDdl index ('DdlConf _ ('Just name) _ _ ('DdlComp _ newTName compName 'Nothing newIndex 'Merge)) : check) other =
    '( 'MergeK index (IndexAction oldTName newTName oldIndex newIndex), check ++ other)

  MigComp ('DdlConf name _ _ _ ('DdlComp _ _ compName _ _ 'Nest))
          ('IxDdl index ('DdlConf name 'Nothing _ _ ('DdlComp _ _ compName 'Nothing _ 'Nest)) : check) other =
    '( 'ActionK 'KeepK index, check ++ other)

  MigComp ('DdlConf oldName _ _ _ ('DdlComp _ _ compName _ _ 'Nest))
          ('IxDdl index ('DdlConf newName ('Just oldName) _ _ ('DdlComp _ _ compName 'Nothing _ 'Nest)) : check) other =
    '( 'ActionK ('RenameK oldName newName) index, check ++ other)

  MigComp
    ('DdlConf oldName oldRename 'False oldConstr ('DdlComp table oldTName oldTypeName oldRenameType indexPrefix oldInc))
    ('IxDdl index ('DdlConf newName newRename delete newConstr ('DdlComp table _ _ ('Just oldTypeName) _ newInc)) : check)
    other
    =
      MigComp
      ('DdlConf oldName oldRename 'False oldConstr ('DdlComp table oldTName oldTypeName oldRenameType indexPrefix oldInc))
      ('IxDdl index ('DdlConf newName newRename delete newConstr ('DdlComp table oldTName oldTypeName 'Nothing indexPrefix newInc)) : check)
        other

  MigComp conf (new : news) other =
    MigComp conf news (new : other)

  MigComp conf '[] other =
    TypeError ("No match for old composite column: " <> conf % other)

type MigPrim :: DdlConf -> [IxDdl] -> [IxDdl] -> (ActionK, [IxDdl])
type family MigPrim conf check other where

  MigPrim ('DdlConf name _ _ _ _) ('IxDdl index ('DdlConf name 'Nothing _ _ ('DdlPrim _ _)) : check) other =
    '( 'ActionK 'KeepK index, check ++ other)

  MigPrim ('DdlConf oldName _ _ _ _) ('IxDdl index ('DdlConf newName ('Just oldName) _ _ ('DdlPrim _ _)) : check) other =
    '( 'ActionK ('RenameK oldName newName) index, check ++ other)

  MigPrim conf (new : news) other =
    MigPrim conf news (new : other)

  MigPrim conf '[] other =
    TypeError ("No match for old primitive column: " <> conf % other)

type MkRemoveAction :: Symbol -> [IxDdl] -> [IxDdl] -> (ActionK, [IxDdl])
type family MkRemoveAction name check other where
  MkRemoveAction name check other =
    '( 'RemoveK name, check ++ other)

-- TODO this reverses the other list every time
type MkMigrationAction :: DdlConf -> DdlConf -> [IxDdl] -> [IxDdl] -> (ActionK, [IxDdl])
type family MkMigrationAction conf conf' check other where

  MkMigrationAction _ ('DdlConf name _ 'True _ _) check other =
    MkRemoveAction name check other

  MkMigrationAction conf ('DdlConf _ _ _ _ ('DdlComp _ _ _ _ _ _)) check other =
    MigComp conf check other

  MkMigrationAction conf ('DdlConf _ _ _ _ ('DdlPrim _ _)) check other =
    MigPrim conf check other

  MkMigrationAction conf _ check other =
    TypeError ("MkMigrationAction:" % conf % check % other)

type NewMigrationActions :: [IxDdl] -> [ActionK]
type family NewMigrationActions cols where
  NewMigrationActions '[] = '[]
  NewMigrationActions ('IxDdl index ('DdlConf name 'Nothing _ _ ('DdlPrim def _)) : news) =
    'ActionK ('AddK name def 'Nothing) index : NewMigrationActions news
  NewMigrationActions ('IxDdl index ('DdlConf name 'Nothing _ _ ('DdlComp _ _ compName _ _ _)) : news) =
    'ActionK ('AddK name 'Nothing ('Just compName)) index : NewMigrationActions news
  NewMigrationActions ('IxDdl index conf : _) =
    TypeError ("NewMigrationActions:" % index % conf)

type MigrationActionsCont :: (ActionK, [IxDdl]) -> [DdlConf] -> [ActionK]
type family MigrationActionsCont result old where
  MigrationActionsCont '(action, unmatchedNew) old = action : MigrationActions old unmatchedNew

-- TODO removing could be done implicitly, given that only renaming really _necessitates_ explicit marking.
-- The only reason to not do that is to avoid mistakes, but that seems exaggerated since we use unit tests for checking
-- consistency anyway, and integration tests to ensure the tables work
type MigrationActions :: [DdlConf] -> [IxDdl] -> [ActionK]
type family MigrationActions old new where
  MigrationActions '[] unmatchedNew =
    NewMigrationActions unmatchedNew
  MigrationActions (cur : old) new =
    MigrationActionsCont (MkMigrationAction cur cur new '[]) old

type ColumnDefault :: Maybe Symbol -> Constraint
class ColumnDefault def where
  columnDefault :: Maybe Text

instance ColumnDefault 'Nothing where
  columnDefault = Nothing

instance KnownSymbol def => ColumnDefault ('Just def) where
  columnDefault = Just (symbolText @def)

type ColumnAddition :: ∀ {ext} . Type -> DdK ext -> Maybe Symbol -> Maybe Symbol -> Constraint
class ColumnAddition tag s comp def where
  columnAddition :: SqelFor tag s -> PgColumnName -> [ColumnAction tag]

-- TODO why is comp not used
-- TODO error message when no migration default was specified for new column without being nullable
instance ColumnDefault def => ColumnAddition tag s comp def where
  columnAddition s n = [AddColumn s n (columnDefault @def)]

type ColIndex :: ∀ {k} . Nat -> [k] -> k -> Constraint
class ColIndex index cols col | index cols -> col where
  colIndex :: NP f cols -> f col

instance ColIndex 0 (col : cols) col where
  colIndex (col :* _) = col

instance {-# overlappable #-} (
    ColIndex (n - 1) cols col
  ) => ColIndex n (c : cols) col where
    colIndex (_ :* cols) = colIndex @(n - 1) cols

type ReifyModAction :: ModK -> Constraint
class ReifyModAction action where
  reifyModAction :: [ColumnAction tag]

instance ReifyModAction 'KeepK where
  reifyModAction = []

instance (
    KnownSymbol oldName,
    KnownSymbol newName
  ) => ReifyModAction ('RenameK oldName newName) where
  reifyModAction =
    [RenameColumn (pgColumnName (symbolText @oldName)) (pgColumnName (symbolText @newName))]

-- TODO use Sqel in actions like for Add actions
type ReifyOldAction :: ∀ {ext} . Type -> ActionK -> DdK ext -> [DdK ext] -> Constraint
class ReifyOldAction tag action old new where
  reifyOldAction :: SqelFor tag old -> NP (SqelFor tag) new -> [ColumnAction tag]

instance (
    -- TODO unused, see above
    -- ColIndex index news new,
    ReifyModAction mod
  ) => ReifyOldAction tag ('ActionK mod index) old news where
  reifyOldAction _ _ =
    reifyModAction @mod

instance (
    ColIndex index news new,
    colsOld ~ DdSub old,
    colsNew ~ DdSub new,
    ReifyModAction indexAction,
    ColumnsChanges tag colsOld colsNew
  ) => ReifyOldAction tag ('MergeK index indexAction) old news where
  reifyOldAction old new =
    reifyModAction @indexAction <> columnsChanges (sqelSub old) (sqelSub (colIndex @index new))

instance (
    KnownSymbol name
  ) => ReifyOldAction tag ('RemoveK name) old new where
  reifyOldAction _ _ =
    [RemoveColumn (pgColumnName (symbolText @name))]

-- -- TODO this has to check that new columns in composite types are
-- -- a) at the end of the list
-- -- b) Maybe
type ReifyNewAction :: ∀ {ext} . Type -> ActionK -> [DdK ext] -> Constraint
class ReifyNewAction tag action new where
  reifyNewAction :: NP (SqelFor tag) new -> [ColumnAction tag]

instance (
    ColIndex index news new,
    ColumnAddition tag new comp def,
    KnownSymbol name
  ) => ReifyNewAction tag ('ActionK ('AddK name def comp) index) news where
  reifyNewAction news =
    columnAddition @tag @new @comp @def (colIndex @index news) (pgColumnName (symbolText @name))

type ReifyActions :: ∀ {ext} . Type -> [ActionK] -> [DdK ext] -> [DdK ext] -> Constraint
class ReifyActions tag actions old new where
  reifyActions :: NP (SqelFor tag) old -> NP (SqelFor tag) new -> [ColumnAction tag]

instance ReifyActions tag '[] '[] new where
  reifyActions Nil _ = []

instance (
    ReifyNewAction tag action new,
    ReifyActions tag actions '[] new
  ) => ReifyActions tag (action : actions) '[] new where
    reifyActions Nil new =
      reifyNewAction @tag @action new <> reifyActions @tag @actions Nil new

instance (
    ReifyOldAction tag action o new,
    ReifyActions tag actions old new
  ) => ReifyActions tag (action : actions) (o : old) new where
    reifyActions (o :* old) new =
      reifyOldAction @tag @action o new <> reifyActions @tag @actions old new

type ColumnsChanges :: ∀ {ext} . Type -> [DdK ext] -> [DdK ext] -> Constraint
class ColumnsChanges tag old new where
  columnsChanges :: NP (SqelFor tag) old -> NP (SqelFor tag) new -> [ColumnAction tag]

-- TODO maybe this could generate two lists, one for old, then use the results for old to generate the new actions.
-- then Reify can do zipped induction for both and skip the new columns that are handled in old, making the index
-- unnecessary.
instance (
    actions ~ MigrationActions (GetDdlMods old) (MkIxDdl 0 (GetDdlMods new)),
    ReifyActions tag actions old new
  ) => ColumnsChanges tag old new where
      columnsChanges old new = reifyActions @tag @actions old new
