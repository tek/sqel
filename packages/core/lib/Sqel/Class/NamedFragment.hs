module Sqel.Class.NamedFragment where

import Data.Type.Bool (type (||))
import Fcf (Eval, Exp, TyEq)
import Generics.SOP (NP)
import Type.Errors (ErrorMessage)

import Sqel.Data.Dd (DdK)
import Sqel.Dd (DdKNames, DdName, DdTypeName, DdTypeNames)
import Sqel.Kind.List (type (++))
import Sqel.SOP.Error (BulletedLines, Quoted, QuotedError, StuckError)
import Sqel.SOP.HFind (HFind (HFindT, hfind))

type ClassName :: Symbol -> Symbol
type family ClassName desc where
  ClassName "table" = "NamedTable"
  ClassName "projection" = "NamedProjection"

type Abstract :: Symbol -> Symbol -> [DdK ext] -> ErrorMessage
type family Abstract desc name frags where
  Abstract desc name frags =
    "The statement mentions a " <> desc <> " named " <> Quoted name <> ", but the " <> desc <> "s are abstract." %
    "You can add a constraint: " <> QuotedError (ClassName desc <> " \"" <> name <> "\" " <> frags)

type NotFound :: Symbol -> Symbol -> [Symbol] -> ErrorMessage
type family NotFound desc name avail where
  NotFound desc name avail =
    "No " <> desc <> " named " <> Quoted name <> ". Available " <> desc <> "s:" % BulletedLines avail %
    "To refer to a " <> desc <> " starting with an uppercase letter, prefix it with " <> Quoted "_" <> "."

type NoField :: Symbol -> Symbol -> [Symbol] -> [DdK ext] -> k
type family NoField desc name avail frags where
  NoField desc name (avail0 : avail) frags =
    StuckError avail0
    (Abstract desc name frags)
    (NotFound desc name (avail0 : avail))

data MatchName :: Symbol -> DdK ext -> Exp Bool
type instance Eval (MatchName name s) =
  Eval (TyEq name (DdName s)) || Eval (TyEq name ("_" ++ DdName s))

data MatchTypeName :: Symbol -> DdK ext -> Exp Bool
type instance Eval (MatchTypeName name s) =
  Eval (TyEq name (DdTypeName s)) || Eval (TyEq name ("_" ++ DdTypeName s))

-- TODO what about an error when the shape of @proj@ is abstract?
type NamedProjection :: ∀ {ext} . Symbol -> [DdK ext] -> DdK ext -> Constraint
class NamedProjection name projs proj | name projs -> proj where
  namedProjection :: NP f projs -> f proj

instance (
    ss ~ s0 : sn,
    err ~ NoField "projection" name (DdKNames ss) ss,
    pred ~ MatchName name,
    HFind err pred ss,
    proj ~ HFindT (MatchName name) (s0 : sn)
  ) => NamedProjection name (s0 : sn) proj where
    namedProjection = hfind @err @pred

type NamedTable :: ∀ {ext} . Symbol -> [DdK ext] -> DdK ext -> Constraint
class NamedTable name tables table | name tables -> table where
  namedTable :: NP f tables -> f table

instance (
    ss ~ s0 : sn,
    err ~ NoField "table" name (DdTypeNames ss) ss,
    pred ~ MatchTypeName name,
    HFind err pred ss,
    table ~ HFindT (MatchTypeName name) (s0 : sn)
  ) => NamedTable name (s0 : sn) table where
    namedTable = hfind @err @pred
