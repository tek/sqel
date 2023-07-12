module Sqel.Migration.Type where

import Generics.SOP (NP (Nil, (:*)))

import Sqel.Data.Dd (Dd (Dd), Struct (Comp))
import Sqel.Data.Migration (ColumnAction, CompAction, TableAction, TypeAction (AddAction, ModifyAction, RenameAction))
import Sqel.Data.PgTypeName (PgCompName, PgTypeNameSym (pgTypeNameSym))
import Sqel.Data.Sqel (SqelFor, sqelSub)
import Sqel.Dd (DdSub)
import Sqel.Kind.List (type (++))
import qualified Sqel.Migration.Column as Column
import Sqel.Migration.Column (
  ColIndex (colIndex),
  ColumnsChanges (columnsChanges),
  IndexAction,
  IndexActionDd,
  IxDdl (IxDdl),
  MkIxDdl,
  )
import Sqel.Migration.Ddl (DdlConf (DdlConf), DdlStruct (DdlComp), GetDdlConf, GetDdlMods)

data ModK =
  KeepK { name :: Symbol }
  |
  AddK { name :: Symbol }
  |
  RenameK { old :: Symbol, new :: Symbol }

data ActionK =
  ActionK {
    mod :: ModK,
    index :: Nat,
    indexAction :: Column.ModK
  }
  |
  UnusedK

type ActionType :: DdlConf -> [IxDdl] -> [IxDdl] -> (ActionK, [IxDdl])
type family ActionType old check other where

  ActionType _ '[] other =
    '( 'UnusedK, other)

  ActionType
    ('DdlConf _ _ _ _ ('DdlComp table oldTName typeName _ oldIndex _))
    ('IxDdl index ('DdlConf _ _ _ _ ('DdlComp table newTName typeName 'Nothing newIndex _)) : news)
    other
    =
    '( 'ActionK ('KeepK typeName) index (IndexAction oldTName newTName oldIndex newIndex), news ++ other)

  ActionType
    ('DdlConf _ _ _ _ ('DdlComp table oldTName oldTypeName _ oldIndex _))
    ('IxDdl index ('DdlConf _ _ _ _ ('DdlComp table newTName newTypeName ('Just oldTypeName) newIndex _)) : news)
    other
    =
    '( 'ActionK ('RenameK oldTypeName newTypeName) index (IndexAction oldTName newTName oldIndex newIndex), news ++ other)

  ActionType old (new : news) other =
    ActionType old news (new : other)

type ActionsNew :: [IxDdl] -> [ActionK]
type family ActionsNew cols where
  ActionsNew '[] = '[]
  ActionsNew ('IxDdl index ('DdlConf _ _ _ _ ('DdlComp 'False _ typeName 'Nothing _ _)) : news) =
    'ActionK ('AddK typeName) index 'Column.KeepK : ActionsNew news
  ActionsNew ('IxDdl index ('DdlConf _ _ _ _ s) : _) =
    TypeError ("ActionsNew:" % index % s)

type ActionsCont :: (ActionK, [IxDdl]) -> [DdlConf] -> [ActionK]
type family ActionsCont cur old where
  ActionsCont '(cur, new) old = cur : TypeMigrationActions old new

type TypeMigrationActions :: [DdlConf] -> [IxDdl] -> [ActionK]
type family TypeMigrationActions old new where
  TypeMigrationActions '[] rest =
    ActionsNew rest
  TypeMigrationActions (old : olds) new =
    ActionsCont (ActionType old new '[]) olds

type TableActionResult :: (ActionK, [IxDdl]) -> ModK
type family TableActionResult result where
  TableActionResult '( 'ActionK action _ _, '[]) = action
  TableActionResult '(_, s) = TypeError ("Table migration didn't match:" % s)

-- TODO unify with Type again
type TableMigrationActions :: DdlConf -> DdlConf -> (ModK, Column.ModK)
type family TableMigrationActions old new where
  TableMigrationActions old new = '(TableActionResult (ActionType old '[ 'IxDdl 0 new] '[]), IndexActionDd old new)

type TypeColumnsChanges :: ∀ {ext} . Type -> Dd ext -> Dd ext -> Constraint
class TypeColumnsChanges tag old new where
  typeColumnsChanges :: SqelFor tag old -> SqelFor tag new -> [ColumnAction tag]

instance (
    colsOld ~ DdSub old,
    colsNew ~ DdSub new,
    ColumnsChanges tag colsOld colsNew
  ) => TypeColumnsChanges tag old new where
    typeColumnsChanges old new = columnsChanges @tag (sqelSub old) (sqelSub new)

type ReifyKeepAction :: ∀ {ext} . Type -> Bool -> Symbol -> Column.ModK -> Dd ext -> Dd ext -> Constraint
class ReifyKeepAction tag table tname indexAction old new where
  reifyKeepAction :: SqelFor tag old -> SqelFor tag new -> TypeAction tag table

instance (
    PgTypeNameSym table tname,
    TypeColumnsChanges tag old new,
    Column.ReifyModAction indexAction
  ) => ReifyKeepAction tag table tname indexAction old new where
    reifyKeepAction old new =
      ModifyAction (pgTypeNameSym @table @tname) (Column.reifyModAction @indexAction <> typeColumnsChanges @tag @old @new old new)

type ReifyModResult :: Type -> Bool -> Type
type family ReifyModResult tag table where
  ReifyModResult tag 'False =
    [(PgCompName, CompAction tag)]
  ReifyModResult tag 'True =
    TypeAction tag 'True

type ReifyModAction :: ∀ {ext} . Type -> Bool -> ModK -> Column.ModK -> Dd ext -> Dd ext -> Constraint
class ReifyModAction tag table action indexAction old new where
  reifyModAction :: SqelFor tag old -> SqelFor tag new -> ReifyModResult tag table

instance (
    ReifyKeepAction tag 'True tname indexAction old new
  ) => ReifyModAction tag 'True ('KeepK tname) indexAction old new where
    reifyModAction old new =
      reifyKeepAction @tag @'True @tname @indexAction old new

instance (
    ReifyKeepAction tag 'False tname indexAction old new,
    PgTypeNameSym 'False tname
  ) => ReifyModAction tag 'False ('KeepK tname) indexAction old new where
    reifyModAction old new =
      [(pgTypeNameSym @'False @tname, reifyKeepAction @tag @'False @tname @indexAction old new)]

instance (
    PgTypeNameSym 'False tname,
    PgTypeNameSym 'False tnameNew,
    TypeColumnsChanges tag old new
  ) => ReifyModAction tag 'False ('RenameK tname tnameNew) indexAction old new where
    reifyModAction old new =
      [(pgTypeNameSym @'False @tname, RenameAction (pgTypeNameSym @'False @tnameNew) (typeColumnsChanges @tag old new))]

type ReifyOldAction :: ∀ {ext} . Type -> Bool -> ActionK -> Dd ext -> [Dd ext] -> Constraint
class ReifyOldAction tag table action old new where
  reifyOldAction :: SqelFor tag old -> NP (SqelFor tag) new -> ReifyModResult tag table

instance (
    ColIndex index news new,
    ReifyModAction tag table mod indexAction old new
  ) => ReifyOldAction tag table ('ActionK mod index indexAction) old news where
  reifyOldAction old new =
    reifyModAction @tag @table @mod @indexAction old (colIndex @index new)

-- -- TODO this has to check that new columns in composite types are
-- -- a) at the end of the list
-- -- b) Maybe
type ReifyNewAction :: ∀ {ext} . ActionK -> [Dd ext] -> Constraint
class ReifyNewAction action new where
  reifyNewAction :: NP (SqelFor tag) new -> (PgCompName, CompAction tag)

instance (
    ColIndex index news new,
    new ~ 'Dd ext a ('Comp tsel c i sub),
    PgTypeNameSym 'False tname
  ) => ReifyNewAction ('ActionK ('AddK tname) index indexAction) news where
  reifyNewAction new =
    (pgTypeNameSym @'False @tname, AddAction (colIndex @index new))

type ReifyActions :: ∀ {ext} . Type -> [ActionK] -> [Dd ext] -> [Dd ext] -> Constraint
class ReifyActions tag actions old new where
  reifyActions :: NP (SqelFor tag) old -> NP (SqelFor tag) new -> [(PgCompName, CompAction tag)]

instance ReifyActions tag '[] '[] new where
  reifyActions =
    mempty

instance (
    ReifyNewAction action new,
    ReifyActions tag actions '[] new
  ) => ReifyActions tag (action : actions) '[] new where
    reifyActions Nil new =
      reifyNewAction @action new : reifyActions @tag @actions Nil new

instance (
    ReifyOldAction tag 'False action o new,
    ReifyActions tag actions old new
  ) => ReifyActions tag (action : actions) (o : old) new where
    reifyActions (o :* old) new =
      reifyOldAction @tag @'False @action o new <> reifyActions @tag @actions old new

type TypeChanges :: Type -> [Dd ext] -> [Dd ext] -> Constraint
class TypeChanges tag old new where
  typeChanges :: NP (SqelFor tag) old -> NP (SqelFor tag) new -> [(PgCompName, CompAction tag)]

instance (
    oldDdl ~ GetDdlMods old,
    newDdl ~ GetDdlMods new,
    actions ~ TypeMigrationActions oldDdl (MkIxDdl 0 newDdl),
    ReifyActions tag actions old new
  ) => TypeChanges tag old new where
    typeChanges old new = reifyActions @tag @actions old new

type ReifyTableAction :: ∀ {ext} . Type -> ModK -> Column.ModK -> Dd ext -> Dd ext -> Constraint
class ReifyTableAction tag action indexAction old new where
  reifyTableAction :: SqelFor tag old -> SqelFor tag new -> TableAction tag

instance (
    ReifyKeepAction tag 'True tname indexAction old new
  ) => ReifyTableAction tag ('KeepK tname) indexAction old new where
    reifyTableAction old new =
      reifyKeepAction @tag @'True @tname @indexAction old new

-- TODO would be nice to use that feature with a different tag for SqelFor that reifies as migrations
type TableChange :: ∀ {ext} . Type -> Dd ext -> Dd ext -> Constraint
class TableChange tag old new where
  tableChange :: SqelFor tag old -> SqelFor tag new -> TableAction tag

-- TODO could go directly to ColumnChanges here, only checking that the name hasn't changed
instance (
    oldDdl ~ GetDdlConf 'True old,
    newDdl ~ GetDdlConf 'True new,
    '(tableAction, indexAction) ~ TableMigrationActions oldDdl newDdl,
    ReifyTableAction tag tableAction indexAction old new
  ) => TableChange tag old new where
    tableChange old new =
      reifyTableAction @tag @tableAction @indexAction old new
