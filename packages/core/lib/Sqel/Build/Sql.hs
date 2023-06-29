module Sqel.Build.Sql where

import qualified Exon
import Exon (exon)
import Generics.SOP (All, K (K), hcmap, hcollapse)

import Sqel.Build.Condition (conditionsClause)
import Sqel.Build.Create (createTableClause, createTypeClause)
import Sqel.Build.DoUpdateSet (doUpdateSetClause)
import Sqel.Build.Drop (dropTableClause)
import Sqel.Build.Insert (insertIntoClause)
import Sqel.Build.Limit (limitClause)
import Sqel.Build.Offset (offsetClause)
import Sqel.Build.OnConflict (onConflictClause)
import Sqel.Build.Order (orderByClause)
import Sqel.Build.Select (selectorsClause)
import Sqel.Build.Table (bindClause)
import Sqel.Build.Values (valuesClause)
import Sqel.Class.ClauseKeyword (ClauseKeyword (clauseKeyword))
import Sqel.Class.DefaultFields (DefaultFields, defaultFields)
import Sqel.Class.Query (FragmentsDd, withFragmentsDd)
import Sqel.Data.Clause (Clause, Clauses, pattern UnClauses, clauseFields)
import Sqel.Data.ClauseConfig (ClauseFieldsFor)
import Sqel.Data.Fragments (Fragments)
import Sqel.Data.Sql (Sql)
import Sqel.Default (
  CreateTable,
  CreateType,
  Def,
  DeleteFrom,
  DoUpdateSet,
  DropTable,
  From,
  InsertInto,
  Join,
  Limit,
  Offset,
  On,
  OnConflict,
  OrderBy,
  Returning,
  Select,
  Values,
  Where,
  )

type BuildClauseDef :: Type -> Constraint
class BuildClauseDef clause where
  buildClauseDef :: Bool -> ClauseFieldsFor Def clause -> Sql

instance BuildClauseDef Select where
  buildClauseDef = selectorsClause

instance BuildClauseDef DeleteFrom where
  buildClauseDef = bindClause

instance BuildClauseDef From where
  buildClauseDef = bindClause

instance BuildClauseDef Join where
  buildClauseDef = bindClause

instance BuildClauseDef InsertInto where
  buildClauseDef _ = insertIntoClause

instance BuildClauseDef Values where
  buildClauseDef _ = valuesClause

instance BuildClauseDef OnConflict where
  buildClauseDef _ = onConflictClause

instance BuildClauseDef DoUpdateSet where
  buildClauseDef _ = doUpdateSetClause

instance BuildClauseDef Returning where
  buildClauseDef = selectorsClause

instance BuildClauseDef CreateTable where
  buildClauseDef _ = createTableClause

instance BuildClauseDef DropTable where
  buildClauseDef _ = dropTableClause

instance BuildClauseDef CreateType where
  buildClauseDef _ = createTypeClause

instance BuildClauseDef OrderBy where
  buildClauseDef = orderByClause

instance BuildClauseDef Limit where
  buildClauseDef = limitClause

instance BuildClauseDef Offset where
  buildClauseDef = offsetClause

------------------------------------------------------------------------------------------------------------------------

type BuildClauseDefMaybe :: Type -> Constraint
class BuildClauseDefMaybe clause where
  buildClauseDefMaybe :: Bool -> ClauseFieldsFor Def clause -> Maybe Sql

instance {-# overlappable #-} (
    BuildClauseDef clause
  ) => BuildClauseDefMaybe clause where
    buildClauseDefMaybe multi fields =
      Just (buildClauseDef @clause multi fields)

instance BuildClauseDefMaybe Where where
  buildClauseDefMaybe = conditionsClause

instance BuildClauseDefMaybe On where
  buildClauseDefMaybe = conditionsClause

------------------------------------------------------------------------------------------------------------------------

withKeyword ::
  ∀ clause .
  ClauseKeyword clause =>
  Maybe Sql ->
  Maybe Sql
withKeyword =
  fmap (\ args -> [exon|#{clauseKeyword @clause} #{args}|])

type BuildClause :: Type -> Type -> Constraint
class BuildClause tag clause where
  buildClause :: Bool -> ClauseFieldsFor tag clause -> Maybe Sql

instance (
    BuildClauseDefMaybe clause,
    ClauseKeyword clause
  ) => BuildClause Def clause where
    buildClause multi fields = withKeyword @clause (buildClauseDefMaybe @clause multi fields)

type BuildClauseViaDefault :: Type -> Type -> Constraint
class BuildClauseViaDefault tag clause where
  buildClauseViaDefault :: Bool -> ClauseFieldsFor tag clause -> Maybe Sql

instance (
    fields ~ ClauseFieldsFor tag clause,
    fieldsDef ~ ClauseFieldsFor Def clause,
    BuildClause Def clause,
    DefaultFields fields fieldsDef
  ) => BuildClauseViaDefault tag clause where
  buildClauseViaDefault multi =
    buildClause @Def @clause multi . defaultFields @fields @fieldsDef

------------------------------------------------------------------------------------------------------------------------

type BuildClauses :: Type -> [Type] -> Constraint
type family BuildClauses tag clauses where
  BuildClauses _ '[] = ()
  BuildClauses tag (c : cs) = (BuildClause tag c, BuildClauses tag cs)

-- TODO remove
type BuildClauseK :: Type -> Type -> Constraint
class BuildClauseK tag clause where
  buildClauseK :: Bool -> Clause tag clause -> Maybe Sql

instance (
    BuildClause tag clause
  ) => BuildClauseK tag clause where
    buildClauseK multi clause = buildClause @tag @clause multi (clauseFields clause)

------------------------------------------------------------------------------------------------------------------------

type BuildSql :: Type -> [Type] -> Constraint
class BuildSql tag cs where
  buildSql :: Bool -> Clauses tag cs result a -> Sql

instance All (BuildClauseK tag) cs => BuildSql tag cs where
  buildSql multi (UnClauses cs) =
    Exon.intercalate " " (catMaybes (hcollapse (hcmap (Proxy @(BuildClauseK tag)) (K . buildClauseK multi) cs)))

buildSqlDd ::
  ∀ query tables tag cs result a .
  FragmentsDd tag query tables =>
  BuildSql tag cs =>
  (Fragments tag query tables '[] -> Clauses tag cs result a) ->
  Sql
buildSqlDd f =
  withFragmentsDd @tag @query @tables \ frags -> buildSql frags.multi (f frags)
