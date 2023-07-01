module Sqel.Class.ReifyComp where

import Data.Some (Some (Some))

import Sqel.Class.DemoteConstraints (DemoteConstraints (demoteConstraints))
import Sqel.Class.ReifyPrim (MkQueryMeta (queryMeta))
import Sqel.Constraints (ConstraintsFor)
import Sqel.Data.Dd (Inc, PrimType (Cond), Sort (Con, Prod, Sum))
import Sqel.Data.IndexState (IndexState)
import Sqel.Data.PgType (pgColumnNameSym, pgTypeRefSym)
import Sqel.Data.PgTypeName (PgTableName, TselPgTypeName (tselPgTypeName))
import Sqel.Data.Sel (PathsL, PathsNameOr, PrefixedIndex, TSel, TSelName, TSelTypeName)
import Sqel.Data.Spine (CompFor, CompSort (CompCon, CompProd, CompSum), spinePath)
import Sqel.Dd (ExtMods, ExtPath)
import qualified Sqel.Default
import Sqel.Default (CompMeta (CompMeta), Def, PrimMeta (PrimMeta))
import Sqel.SOP.Constraint (KnownSymbols)

type DemoteSort :: ∀ {ext} . Type -> Sort -> Symbol -> ext -> Constraint
class DemoteSort tag c tname ext where
  demoteSort :: PgTableName -> IndexState (CompSort tag)

instance (
    prePath ~ PathsL (ExtPath ext),
    name ~ PrefixedIndex prefix tname,
    indexPath ~ name : prePath,
    KnownSymbol name,
    KnownSymbols indexPath
  ) => DemoteSort Def ('Sum prefix) tname ext where
    demoteSort table = do
      query <- queryMeta @'[] @'False @'Cond
      pure $ CompSum PrimMeta {
        name = pgColumnNameSym @name,
        path = spinePath @indexPath,
        colType = "bigint",
        table,
        constr = def,
        query
      }

instance DemoteSort tag 'Prod tname path where
  demoteSort _ = pure CompProd

instance DemoteSort tag 'Con tname path where
  demoteSort _ = pure CompCon

type ReifyComp :: ∀ {ext} . Type -> Bool -> ext -> Type -> TSel -> Sort -> Inc -> Constraint
class ReifyComp tag root ext a tsel c i where
  reifyComp :: PgTableName -> CompFor tag

-- TODO the type name thing here is wrong
instance (
    mods ~ ExtMods ext,
    tname ~ TSelName tsel,
    name ~ PathsNameOr tname (ExtPath ext),
    KnownSymbol name,
    constr ~ ConstraintsFor mods,
    DemoteConstraints constr,
    pgName ~ TSelTypeName tsel,
    TselPgTypeName tsel root typeName,
    KnownSymbol pgName
  ) => ReifyComp Def root ext a tsel c i where
    reifyComp table =
      CompMeta {
        name = pgColumnNameSym @name,
        typeName = Some (tselPgTypeName @tsel @root),
        colType = pgTypeRefSym @pgName,
        table,
        constr = demoteConstraints @constr
      }

instance {-# overlappable #-} (
    CompFor tag ~ CompMeta,
    ReifyComp Def root ext a tsel c i
  ) => ReifyComp tag root ext a tsel c i where
    reifyComp = reifyComp @Def @root @ext @a @tsel @c @i
