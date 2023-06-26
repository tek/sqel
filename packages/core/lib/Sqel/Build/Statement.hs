module Sqel.Build.Statement where

import Sqel.Build.Sql (BuildSql (buildSql))
import Sqel.Codec.Result (ClausesResultDecoder (clausesResultDecoder))
import Sqel.Data.Clause (ClauseK, Clauses (Clauses))
import qualified Sqel.Data.Codec
import Sqel.Data.Dd (DdK)
import Sqel.Data.Sqel (SqelFor, sqelCodec)
import Sqel.Data.Statement (Statement (Statement))
import Sqel.Dd (MaybeDdType)
import Sqel.Kind.Maybe (MaybeD (JustD, NothingD))

type BuildStatement :: ∀ {ext} . Type -> Maybe (DdK ext) -> Type -> [ClauseK ext] -> Constraint
class BuildStatement tag query result cs where
  buildStatement ::
    Bool ->
    Clauses tag cs a ->
    MaybeD (SqelFor tag) query ->
    Statement (MaybeDdType query) result

instance (
    ClausesResultDecoder tag result cs,
    BuildSql tag cs
  ) => BuildStatement tag query result cs where
    buildStatement multi cs@(Clauses cl _) query =
      Statement (buildSql @tag multi cs) params row
      where
        row = (clausesResultDecoder cl).decodeValue
        params = case query of
          JustD q -> (sqelCodec q).encoder.encodeValue
          NothingD -> mempty
