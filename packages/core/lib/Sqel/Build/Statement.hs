module Sqel.Build.Statement where

import Sqel.Build.Sql (BuildSql (buildSql))
import Sqel.Codec.Result (ClausesResultDecoder (clausesResultDecoder))
import Sqel.Data.Clause (Clauses (Clauses))
import qualified Sqel.Data.Codec
import Sqel.Data.Dd (DdK)
import Sqel.Data.Sqel (SqelFor, sqelCodec)
import Sqel.Data.Statement (Statement (Statement))
import Sqel.Dd (MaybeDdType)
import Sqel.Kind.Maybe (MaybeD (JustD, NothingD))

type BuildStatement :: ∀ {extq} . Type -> Maybe (DdK extq) -> [Type] -> Maybe [Type] -> Type -> Constraint
class BuildStatement tag query cs results result where
  buildStatement ::
    Bool ->
    Clauses tag cs results a ->
    MaybeD (SqelFor tag) query ->
    Statement (MaybeDdType query) result

instance (
    BuildSql tag cs,
    ClausesResultDecoder results result
  ) => BuildStatement tag query cs results result where
    buildStatement multi cs@(Clauses _ res _) query =
      Statement (buildSql @tag multi cs) params row
      where
        row = (clausesResultDecoder res).decodeValue
        params = case query of
          JustD q -> (sqelCodec q).encoder.encodeValue
          NothingD -> mempty
