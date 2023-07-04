module Sqel.Class.ReifyPrim where

import Sqel.Class.DemoteConstraints (DemoteConstraints (demoteConstraints))
import Sqel.Class.Mods (FindMod, HasMod)
import Sqel.Constraints (ConstraintsFor)
import Sqel.Data.Dd (PrimType (Cond, NoCond))
import Sqel.Data.IndexState (IndexState, withNewIndex)
import Sqel.Data.Mods.CondOp (CondOp)
import Sqel.Data.Mods.Ignore (Ignore)
import Sqel.Data.Mods.Nullable (Nullable)
import Sqel.Data.PgType (pgColumnNameSym)
import Sqel.Data.PgTypeName (PgTableName)
import Sqel.Data.QueryMeta (CondMeta (CondMeta), pattern CondOp, QueryMeta (QueryMeta, QuerySynthetic))
import Sqel.Data.Sel (Paths (Paths))
import Sqel.Data.Spine (PrimFor)
import Sqel.Data.Sql (Sql (Sql))
import Sqel.Dd (ExtMods, ExtPath)
import qualified Sqel.Default
import Sqel.Default (Def, PrimMeta (PrimMeta))
import Sqel.Path (ddPath)
import Sqel.Reify.PrimName (PrimName, reifyPrimName)
import Sqel.SOP.Constraint (KnownSymbols, symbolText)
import Sqel.SOP.HasGeneric (BoolVal (boolVal))

type DemoteNullable :: Maybe Bool -> Constraint
class DemoteNullable mod where
  demoteNullable :: Maybe Bool

instance (
    BoolVal guard
  ) => DemoteNullable ('Just guard) where
    demoteNullable = Just (boolVal @guard)

instance DemoteNullable 'Nothing where
  demoteNullable = Nothing

type DemoteOp :: Maybe Symbol -> Constraint
class DemoteOp mod where
  demoteOp :: Sql

instance DemoteOp 'Nothing where
  demoteOp = "="

instance KnownSymbol op => DemoteOp ('Just op) where
  demoteOp = Sql (symbolText @op)

-- TODO 'False 'Cond – error?
type MkQueryMeta :: [Type] -> Bool -> PrimType -> Constraint
class MkQueryMeta mods ignore prim where
  queryMeta :: IndexState QueryMeta

instance MkQueryMeta mods 'True 'NoCond where
  queryMeta = pure QuerySynthetic

instance MkQueryMeta mods 'False 'NoCond where
  queryMeta =
    withNewIndex \ index -> QueryMeta index Nothing

instance (
    nullable ~ FindMod Nullable mods,
    op ~ FindMod CondOp mods,
    DemoteNullable nullable,
    DemoteOp op
  ) => MkQueryMeta mods 'False 'Cond where
    queryMeta =
      withNewIndex \ index -> QueryMeta index (Just (CondMeta (CondOp (demoteOp @op)) (demoteNullable @nullable)))

type ReifyPrim :: ∀ {ext} . Type -> ext -> Type -> PrimType -> Constraint
class ReifyPrim tag ext a prim where
  reifyPrim :: PgTableName -> IndexState (PrimFor tag)

instance (
    'Paths name dd tablePath ~ ExtPath ext,
    mods ~ ExtMods ext,
    KnownSymbols tablePath,
    KnownSymbol name,
    KnownSymbol (PrimName Def a mods),
    constr ~ ConstraintsFor mods,
    DemoteConstraints constr,
    ignore ~ HasMod Ignore mods,
    MkQueryMeta mods ignore prim
  ) => ReifyPrim Def ext a prim where
  reifyPrim table = do
    query <- queryMeta @mods @ignore @prim
    pure PrimMeta {
      name = pgColumnNameSym @name,
      path = ddPath @tablePath,
      colType = reifyPrimName @Def @a @mods,
      table,
      constr = demoteConstraints @constr,
      query
    }

instance {-# overlappable #-} (
    PrimFor tag ~ PrimMeta,
    ReifyPrim Def ext a prim
  ) => ReifyPrim tag ext a prim where
    reifyPrim = reifyPrim @Def @ext @a @prim
