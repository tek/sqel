module Sqel.Class.ReifyPrim where

import Sqel.Class.DemoteConstraints (DemoteConstraints (demoteConstraints))
import Sqel.Class.Mods (FindMod)
import Sqel.Constraints (ConstraintsFor)
import Sqel.Data.Dd (PrimType (Cond, NoCond))
import Sqel.Data.Mods.Nullable (Nullable)
import Sqel.Data.PgType (pgColumnNameSym)
import Sqel.Data.PgTypeName (PgTableName)
import Sqel.Data.Sel (IxPaths (IxPaths), Paths (Paths))
import Sqel.Data.Spine (PrimFor, spinePath)
import Sqel.Dd (ExtMods, ExtPath)
import qualified Sqel.Default
import Sqel.Default (CondMeta (CondMeta), Def, PrimMeta (PrimMeta), QueryMeta (QueryMeta, QuerySynthetic))
import Sqel.Reify.PrimName (PrimName, reifyPrimName)
import Sqel.SOP.Constraint (KnownSymbols, natInt)
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

-- TODO 'Nothing 'Cond – error?
type MkQueryMeta :: [Type] -> Maybe Nat -> PrimType -> Constraint
class MkQueryMeta mods ix prim where
  queryMeta :: QueryMeta

instance MkQueryMeta mods 'Nothing 'NoCond where
  queryMeta = QuerySynthetic

instance KnownNat index => MkQueryMeta mods ('Just index) 'NoCond where
  queryMeta = QueryMeta (natInt @index) Nothing

instance (
    KnownNat index,
    nullable ~ FindMod Nullable mods,
    DemoteNullable nullable
  ) => MkQueryMeta mods ('Just index) 'Cond where
    queryMeta = QueryMeta (natInt @index) (Just (CondMeta "=" (demoteNullable @nullable)))

type ReifyPrim :: ∀ {ext} . Type -> ext -> Type -> PrimType -> Constraint
class ReifyPrim tag ext a prim where
  reifyPrim :: PgTableName -> PrimFor tag

instance (
    path ~ ExtPath ext,
    mods ~ ExtMods ext,
    'IxPaths ('Paths name dd tablePath) index ~ path,
    KnownSymbols tablePath,
    KnownSymbol name,
    KnownSymbol (PrimName Def a mods),
    constr ~ ConstraintsFor mods,
    DemoteConstraints constr,
    MkQueryMeta mods index prim
  ) => ReifyPrim Def ext a prim where
  reifyPrim table =
    PrimMeta {
      name = pgColumnNameSym @name,
      path = spinePath @tablePath,
      colType = reifyPrimName @Def @a @mods,
      table,
      constr = demoteConstraints @constr,
      query = queryMeta @mods @index @prim
    }

instance {-# overlappable #-} (
    PrimFor tag ~ PrimMeta,
    ReifyPrim Def ext a prim
  ) => ReifyPrim tag ext a prim where
    reifyPrim = reifyPrim @Def @ext @a @prim
