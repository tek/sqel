module Sqel.Class.NamedFragment where

import Data.Type.Bool (type (||))
import Fcf (Eval, Exp, TyEq)
import Generics.SOP (NP)
import Type.Errors (ErrorMessage)

import Sqel.Data.Dd (Dd)
import Sqel.Dd (DdName, DdNames, DdTypeName, DdTypeNames)
import Sqel.Kind.List (type (++))
import Sqel.SOP.Error (BulletedLines, Quoted, QuotedError, StuckError)
import Sqel.SOP.HFind (HFind (HFindT, hfind))

type ClassName :: Symbol -> Symbol
type family ClassName desc where
  ClassName "table" = "NamedTable"
  ClassName "projection" = "NamedProjection"

type Abstract :: Symbol -> Symbol -> [Dd] -> ErrorMessage
type family Abstract desc name frags where
  Abstract desc name frags =
    "The statement mentions a " <> desc <> " named " <> Quoted name <> ", but the " <> desc <> "s are abstract." %
    "You can add a constraint: " <> QuotedError (ClassName desc <> " \"" <> name <> "\" " <> frags)

type NotFound :: Symbol -> Symbol -> [Symbol] -> ErrorMessage
type family NotFound desc name avail where
  NotFound desc name avail =
    "No " <> desc <> " named " <> Quoted name <> ". Available " <> desc <> "s:" % BulletedLines avail %
    "To refer to a " <> desc <> " starting with an uppercase letter, prefix it with " <> Quoted "_" <> "."

type NoField :: Symbol -> Symbol -> [Symbol] -> [Dd] -> k
type family NoField desc name avail frags where
  NoField desc name (avail0 : avail) frags =
    StuckError avail0
    (Abstract desc name frags)
    (NotFound desc name (avail0 : avail))

data MatchName :: Symbol -> Dd -> Exp Bool
type instance Eval (MatchName name s) =
  Eval (TyEq name (DdName s)) || Eval (TyEq name ("_" ++ DdName s))

data MatchTypeName :: Symbol -> Dd -> Exp Bool
type instance Eval (MatchTypeName name s) =
  Eval (TyEq name (DdTypeName s)) || Eval (TyEq name ("_" ++ DdTypeName s))

-- TODO what about an error when the shape of @proj@ is abstract?
type NamedProjection :: Symbol -> [Dd] -> Constraint
class NamedProjection name proj where
  type ProjectionNamed name proj :: Dd
  namedProjection :: NP f proj -> f (ProjectionNamed name proj)

instance (
    ss ~ s0 : sn,
    err ~ NoField "projection" name (DdNames ss) ss,
    pred ~ MatchName name,
    HFind err pred ss
  ) => NamedProjection name (s0 : sn) where
    type ProjectionNamed name (s0 : sn) = HFindT (MatchName name) (s0 : sn)
    namedProjection = hfind @err @pred

type NamedTable :: Symbol -> [Dd] -> Constraint
class NamedTable name tables where
  type TableNamed name tables :: Dd
  namedTable :: NP f tables -> f (TableNamed name tables)

instance (
    ss ~ s0 : sn,
    err ~ NoField "table" name (DdTypeNames ss) ss,
    pred ~ MatchTypeName name,
    HFind err pred ss
  ) => NamedTable name (s0 : sn) where
    type TableNamed name (s0 : sn) = HFindT (MatchTypeName name) (s0 : sn)
    namedTable = hfind @err @pred
