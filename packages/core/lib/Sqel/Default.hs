module Sqel.Default (
  Def,
  module Sqel.Default,
  module Sqel.Default.Clauses,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Some (Some)

import Sqel.Data.CondExpr (CondExpr)
import Sqel.Data.Constraints (Constraints)
import Sqel.Data.Dd (Dd)
import Sqel.Data.Def (Def)
import Sqel.Data.Path (FieldPath)
import Sqel.Data.PgType (PgColumnName, PgPrimName, PgTypeRef)
import Sqel.Data.PgTypeName (PgTableName, PgTypeName)
import qualified Sqel.Data.QueryMeta
import Sqel.Data.QueryMeta (pattern CondExpr, pattern CondMeta, QueryMeta (QueryMeta))
import Sqel.Data.Spine (CompFor, PrimFor, Spine)
import Sqel.Data.Sqel (SqelFor)
import Sqel.Default.Clauses
import Sqel.Error.Clause (ClauseDesc, ClauseError)
import Sqel.Error.Fragment (FragmentMismatch, FragmentMismatchDefault)

data PrimMeta =
  PrimMeta {
    name :: PgColumnName,
    path :: FieldPath,
    colType :: PgPrimName,
    table :: PgTableName,
    constr :: Constraints,
    query :: QueryMeta
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

type instance PrimFor Def = PrimMeta

withCondExpr :: CondExpr -> PrimMeta -> PrimMeta
withCondExpr expr = \case
  PrimMeta {query = QueryMeta {cond = Just CondMeta {..}, ..}, ..} ->
    PrimMeta {query = QueryMeta {cond = Just CondMeta {code = CondExpr expr, ..}, ..}, ..}
  m -> m

data CompMeta =
  CompMeta {
    name :: PgColumnName,
    typeName :: Some PgTypeName,
    colType :: PgTypeRef,
    table :: PgTableName,
    constr :: Constraints
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

type instance CompFor Def = CompMeta

type SpineDef = Spine Def

type Sqel :: Dd -> Type
type Sqel = SqelFor Def

-- TODO remove, this has no advantage really
-- Or think of some way that the fragment kind can be used
type instance ClauseError Def clause _ =
  "These fragments are incompatible with a " <> ClauseDesc clause <> "."

type instance FragmentMismatch Def clause sort =
  FragmentMismatchDefault clause sort
