module Sqel.Dsl (
  Prim,
  PrimAs,
  PrimUsing,
  PrimWith,
  PrimAuto,
  Param,
  module Sqel.Dsl.Comp,
  Mod,
  Mods,
  ModWith,
  ModTrans,
  module Sqel.Dsl,
) where

import Data.UUID (UUID)
import Data.Vector (Vector)
import Fcf (Eval, Exp, type (@@))
import Generics.SOP.GGP (GCode, GDatatypeInfoOf)
import qualified Generics.SOP.Type.Metadata as SOP
import Generics.SOP.Type.Metadata (ConstructorInfo (Constructor, Infix, Record), DatatypeInfo (ADT))
import Prelude hiding (Enum, Mod)
import Type.Errors (ErrorMessage)

import Sqel.Data.Dd (
  ConCol,
  Dd (Dd),
  Dd0,
  Dd1,
  Ext0 (Ext0),
  Inc (Merge, Nest),
  PrimType (Cond, NoCond),
  Sort (Con, Prod, Sum),
  Struct (Comp, Prim),
  )
import Sqel.Data.Mods (NoMods)
import qualified Sqel.Data.Mods.Array as Mods
import qualified Sqel.Data.Mods.CondOp as Mods
import qualified Sqel.Data.Mods.Enum as Mods
import qualified Sqel.Data.Mods.Ignore as Mods
import qualified Sqel.Data.Mods.Json as Mods
import Sqel.Data.Mods.MigrationDefault (MigrationDefault)
import Sqel.Data.Mods.MigrationDelete (MigrationDelete)
import Sqel.Data.Mods.MigrationRename (MigrationRename)
import Sqel.Data.Mods.MigrationRenameIndex (MigrationRenameIndex)
import Sqel.Data.Mods.MigrationRenameType (MigrationRenameType)
import Sqel.Data.Mods.Name (SetPrimName)
import qualified Sqel.Data.Mods.Newtype as Mods (Newtype)
import qualified Sqel.Data.Mods.Nullable as Mods (Nullable, NullableConf (NullableConf))
import qualified Sqel.Data.Mods.PrimaryKey as Mods (PrimaryKey)
import qualified Sqel.Data.Mods.TableName as Mods
import qualified Sqel.Data.Mods.Unique as Mods (Unique)
import Sqel.Data.Name (AmendName, Name (Name, NameAuto), NamePrefix (DefaultPrefix, NamePrefix))
import Sqel.Data.Sel (Path (PathSkip), Sel (Sel), SelAuto, TSel (TSel), TSelWithPrefix)
import Sqel.Data.Uid (Uid)
import Sqel.Dd (SetDdName, SetDdPath, SetDdPrefix)
import Sqel.Dsl.Comp
import Sqel.Dsl.Error (TypeNamePrimError)
import Sqel.Dsl.Fields (Field (FieldNum), NamedFields, ReifyFieldNames)
import Sqel.Dsl.Mod (AddMod, AddModWith, AddMods, Mod, ModTrans, ModWith, Mods)
import Sqel.Dsl.Prim (AllAuto, Param, Prim, PrimAs, PrimAuto, PrimEnum, PrimJson, PrimJsonb, PrimUsing, PrimWith)
import Sqel.Kind.Error (PlainTypeError, Quoted)
import Sqel.Migration.Ddl (Ddl, ToDdl)
import Sqel.Normalize (NormalizeDd)

type FromGenF k = DatatypeInfo -> [[Type]] -> Exp k

type FromGen :: ∀ k . FromGenF k -> Type -> k
type family FromGen f a where
  FromGen f a = f (GDatatypeInfoOf a) @@ GCode a

------------------------------------------------------------------------------------------------------------------------

type AmendDdName :: Symbol -> Dd0 -> Dd0
type family AmendDdName name s where
  AmendDdName name ('Dd ('Ext0 ('Sel old path) mods) a s) =
    'Dd ('Ext0 ('Sel (AmendName name old) path) mods) a s

type SetType :: Type -> Dd0 -> Dd0
type family SetType a s where
  SetType a ('Dd ext _ s) = 'Dd ext a s

type family WrapType f s :: Dd0 where
  WrapType f ('Dd ext a s) = 'Dd ext (f a) s

data ApplyTypeName :: Symbol -> Dd0 -> Exp Dd0
type instance Eval (ApplyTypeName name ('Dd ext a ('Comp ('TSel prefix _) c i s))) =
  'Dd ext a ('Comp ('TSel prefix name) c i s)
type instance Eval (ApplyTypeName name ('Dd ext a ('Prim _))) =
  TypeNamePrimError name ext a

type family ApplyTableName' name s :: Dd0 where
  ApplyTableName' name ('Dd ext a ('Comp ('TSel prefix _) c i s)) =
    'Dd ext a ('Comp ('TSel prefix name) c i s)
  ApplyTableName' name ('Dd ('Ext0 ('Sel 'NameAuto path) mods) a s) =
    'Dd ('Ext0 ('Sel ('Name name) path) mods) a s
  ApplyTableName' name prim =
    AddMod (Mods.TableName name) prim

data ApplyTableName :: Symbol -> Dd0 -> Exp Dd0
type instance Eval (ApplyTableName name s) =
  ApplyTableName' name s

data ApplyTypePrefix :: Symbol -> Dd0 -> Exp Dd0
type instance Eval (ApplyTypePrefix prefix ('Dd ext a ('Comp tsel c i s))) =
  'Dd ext a ('Comp (TSelWithPrefix prefix tsel) c i s)

data ApplyIndexPrefix :: Symbol -> Dd0 -> Exp Dd0
type instance Eval (ApplyIndexPrefix prefix ('Dd ext a ('Comp tsel ('Sum _) i s))) =
  'Dd ext a ('Comp tsel ('Sum ('NamePrefix prefix)) i s)

data ApplyName :: Symbol -> Dd0 -> Exp Dd0
type instance Eval (ApplyName name s) =
  SetDdName name s

data ApplyPath :: [Symbol] -> Dd0 -> Exp Dd0
type instance Eval (ApplyPath path s) =
  SetDdPath path s

data ApplyPrefix :: [Symbol] -> Dd0 -> Exp Dd0
type instance Eval (ApplyPrefix pre s) =
  SetDdPrefix pre s

------------------------------------------------------------------------------------------------------------------------

type ApplyNullable :: Bool -> Dd0 -> Dd0
type family ApplyNullable guard s where
  ApplyNullable guard s =
    AddMod (Mods.Nullable ('Mods.NullableConf guard 'Nothing)) s

type ApplyNullableF :: Bool -> (Type -> Type) -> Dd0 -> Dd0
type family ApplyNullableF guard f s where
  ApplyNullableF guard f s =
    AddMod (Mods.Nullable ('Mods.NullableConf guard ('Just f))) (WrapType f s)

-- TODO other types than Maybe should be possible to unwrap with Nullable.
-- It probably should carry Maybe in its type.
-- This should then be generalized with Array.
type ReifyNullable :: Bool -> Type -> Type -> Dd0
type family ReifyNullable guard a spec where
  ReifyNullable guard (Maybe a) spec =
    ApplyNullableF guard Maybe (ReifyE a spec)
  ReifyNullable guard a spec =
    ApplyNullable guard (ReifyE a spec)

type ApplyNewtype :: Type -> Type -> Dd0 -> Dd0
type family ApplyNewtype a w s where
  ApplyNewtype a w s =
    AddMod (Mods.Newtype a w) (SetType a s)

type ReifyNewtypeWith :: Type -> Type -> DatatypeInfo -> [[Type]] -> Dd0
type family ReifyNewtypeWith a spec info ass where
  ReifyNewtypeWith a spec ('SOP.Newtype _ _ _) '[ '[w]] =
    ApplyNewtype a w (ReifyE w spec)
  ReifyNewtypeWith a _ _ _ =
    TypeError (Quoted a <> " is not a newtype.")

data ReifyNewtype :: Type -> Type -> FromGenF Dd0
type instance Eval (ReifyNewtype a spec info ass) =
  ReifyNewtypeWith a spec info ass

type ApplyArray :: (Type -> Type) -> Dd0 -> Dd0
type family ApplyArray f s where
  ApplyArray f s =
    AddMod (Mods.Array f) (WrapType f s)

type ReifyArray :: Type -> (Type -> Type) -> Type -> Dd0
type family ReifyArray a f spec where
  ReifyArray (f a) f spec =
    ApplyArray f (ReifyE a spec)
  ReifyArray a f _ =
    TypeError ("A column was declared as an array of type " <> Quoted f <> ", but its type does not match:" % Quoted a)

------------------------------------------------------------------------------------------------------------------------

type Ignore :: Type -> Type
type Ignore spec = Param (Mod Mods.Ignore spec)

type Unique :: Type -> Type
type Unique = Mod Mods.Unique

type Pk :: Type -> Type
type Pk = Mod Mods.PrimaryKey

type Array :: (Type -> Type) -> Type -> Type
data Array f spec

type Nullable :: Type -> Type
data Nullable spec

type OrNull :: Type -> Type
data OrNull spec

type NewtypeOf :: Type -> Type
data NewtypeOf spec

type Newtype :: ∀ k . k
type family Newtype where
  Newtype = NewtypeOf PrimAuto
  Newtype = NewtypeOf

-- TODO
type Newtypes :: Type
data Newtypes

type Enum :: Type
type Enum = Prim PrimEnum

type TypeName :: Symbol -> Type -> Type
type TypeName name = ModTrans (ApplyTypeName name)

type TableName :: Symbol -> Type -> Type
type TableName name = ModTrans (ApplyTableName name)

type TypePrefix :: Symbol -> Type -> Type
type TypePrefix p = ModTrans (ApplyTypePrefix p)

type IndexPrefix :: Symbol -> Type -> Type
type IndexPrefix p = ModTrans (ApplyIndexPrefix p)

type Rename :: Symbol -> Type -> Type
type Rename name = Mod (MigrationRename name)

type RenameType :: Symbol -> Type -> Type
type RenameType name = Mod (MigrationRenameType name)

type RenameIndex :: Symbol -> Type -> Type
type RenameIndex name = Mod (MigrationRenameIndex name)

type Delete :: Type -> Type
type Delete = Mod MigrationDelete

type Default :: Symbol -> Type -> Type
type Default value = Mod (MigrationDefault value)

type Name :: Symbol -> Type -> Type
type Name name = ModTrans (ApplyName name)

type Path :: [Symbol] -> Type -> Type
type Path pre = ModTrans (ApplyPath pre)

type Prefix :: [Symbol] -> Type -> Type
type Prefix pre = ModTrans (ApplyPrefix pre)

type Json :: Type
type Json = Prim PrimJson

type Jsonb :: Type
type Jsonb = Prim PrimJsonb

type CondOp :: Symbol -> Type -> Type
type CondOp op = Mod (Mods.CondOp op)

type Equal :: Type -> Type
type Equal = CondOp "="

type Lt :: Type -> Type
type Lt = CondOp "<"

type LEq :: Type -> Type
type LEq = CondOp "<="

type Gt :: Type -> Type
type Gt = CondOp ">"

type GEq :: Type -> Type
type GEq = CondOp ">="

------------------------------------------------------------------------------------------------------------------------

type SetNoCond :: Dd0 -> Dd0
type family SetNoCond s where
  SetNoCond ('Dd ext a ('Prim _)) = 'Dd ext a ('Prim 'NoCond)
  SetNoCond ('Dd _ a ('Comp _ _ _ _)) =
    TypeError (
      "Cannot mark composite type " <> Quoted a <> " as a " <> Quoted Param <> "." %
      "This modifier means that the query field will be skipped in condition clauses like" %
      Quoted "where" <> ", which only applies to primitive columns."
    )

type CompDd :: Type -> TSel -> Sort -> Inc -> [Dd0] -> Dd0
type CompDd a tsel sort inc sub = 'Dd ('Ext0 SelAuto NoMods) a ('Comp tsel sort inc sub)

type PrimDd :: Type -> Dd0
type PrimDd a = 'Dd ('Ext0 SelAuto NoMods) a ('Prim 'Cond)

type PrimBasic :: Type -> Type -> Dd0
type family PrimBasic a prim where
  PrimBasic a PrimAuto = PrimDd a
  PrimBasic a PrimEnum = AddMods [SetPrimName "text", Mods.Enum] (PrimDd a)
  PrimBasic a PrimJson = AddMod Mods.Json (PrimDd a)
  PrimBasic a PrimJsonb = AddMod Mods.Jsonb (PrimDd a)
  PrimBasic a (NewtypeOf spec) = FromGen (ReifyNewtype a spec) a

type PrimInfer :: Type -> Type -> Dd0
type family PrimInfer a prim where
  PrimInfer (Maybe a) prim = ApplyNullableF 'False Maybe (PrimInfer a prim)
  PrimInfer [a] prim = ApplyArray [] (PrimInfer a prim)
  PrimInfer (NonEmpty a) prim = ApplyArray NonEmpty (PrimInfer a prim)
  PrimInfer (Seq a) prim = ApplyArray Seq (PrimInfer a prim)
  PrimInfer (Vector a) prim = ApplyArray Vector (PrimInfer a prim)
  PrimInfer (Set a) prim = ApplyArray Set (PrimInfer a prim)
  PrimInfer a prim = PrimBasic a prim

type AutoPrim :: Type -> Type
type family AutoPrim spec where
  AutoPrim Gen = PrimAuto
  AutoPrim Newtypes = Newtype PrimAuto

------------------------------------------------------------------------------------------------------------------------

type ProdCol :: Symbol -> Type -> Type -> Dd0
type family ProdCol name a spec where
  ProdCol name a spec = AmendDdName name (ReifyE a spec)

-- TODO include specifics in count mismatch error
type ProdCols :: [Symbol] -> [Type] -> [Type] -> [Dd0]
type family ProdCols fields as cols where
  ProdCols '[] '[] '[] = '[]
  ProdCols (name : fields) (a : as) (col : cols) = ProdCol name a col : ProdCols fields as cols
  ProdCols _ _ _ = PlainTypeError "The number of specified columns does not match the data type."

type ProdSort :: Sort -> Type -> [Type] -> Symbol -> [Field] -> [Type] -> Dd0
type family ProdSort sort a as name fields cols where
  ProdSort sort a as name fields cols =
    CompDd a ('TSel 'DefaultPrefix name) sort 'Nest (ProdCols (ReifyFieldNames name fields) as cols)

type ProdInfo :: Type -> [[Type]] -> DatatypeInfo -> [Type] -> Dd0
type family ProdInfo a as info cols where
  ProdInfo a '[as] ('ADT _ name '[ 'Record _ fields] _) cols =
    ProdSort 'Prod a as name (NamedFields fields) cols
  ProdInfo a _ _ _ =
    TypeError ("Prod: Not a type with a single record constructor: " <> Quoted a)

type ProdGen :: Type -> [Type] -> Dd0
type family ProdGen a cols where
  ProdGen a cols = ProdInfo a (GCode a) (GDatatypeInfoOf a) cols

------------------------------------------------------------------------------------------------------------------------

type ConMeta :: Symbol -> [Type] -> [Field] -> Type
data ConMeta name as fields

-- TODO these seem redundant
type ConAutoCols :: Type -> [ConstructorInfo] -> [[Type]] -> [Type]
type family ConAutoCols spec cons ass where
  ConAutoCols _ '[] _ = '[]
  ConAutoCols spec ('Constructor _ : cons) ('[_] : ass) = spec : ConAutoCols spec cons ass
  ConAutoCols spec ('Record _ _ : cons) ('[_] : ass) = spec : ConAutoCols spec cons ass
  ConAutoCols spec ('Constructor _ : cons) (_ : ass) = spec : ConAutoCols spec cons ass
  ConAutoCols spec ('Record _ _ : cons) (_ : ass) = spec : ConAutoCols spec cons ass

type SetConColsNullable :: [Dd0] -> [Dd0]
type family SetConColsNullable cols where
  SetConColsNullable '[] = '[]
  SetConColsNullable (s : ss) = ApplyNullable 'False s : SetConColsNullable ss

type SetConNullable :: Sort -> [Dd0] -> [Dd0]
type family SetConNullable con1 s where
  SetConNullable 'Con cols = SetConColsNullable cols
  SetConNullable _ cols = cols

type ConFor :: Symbol -> [Type] -> [Field] -> [Type] -> Dd0
type family ConFor name as fields cols where
  ConFor name as fields cols =
    ProdSort 'Con (ConCol as) as name fields cols

type ConPrims :: Type -> Symbol -> [Type] -> [Field] -> Dd0
type family ConPrims spec name as fields where
  ConPrims spec name as fields =
    ProdSort 'Con (ConCol as) as name fields (AllAuto (AutoPrim spec) as)

------------------------------------------------------------------------------------------------------------------------

type SumCon :: Symbol -> [Type] -> [Field] -> Type -> Dd0
type family SumCon name as fields spec where
  SumCon name as fields spec = AmendDdName name (ReifyE (ConMeta name as fields) spec)

type ConstructorFields :: Symbol -> Nat -> [Type] -> [Field]
type family ConstructorFields name index ass where
  ConstructorFields _ _ '[] = '[]
  ConstructorFields name n (_ : as) = 'FieldNum n : ConstructorFields name (n + 1) as

type SumCons :: [ConstructorInfo] -> [[Type]] -> [Type] -> [Dd0]
type family SumCons cons ass conSpecs where
  SumCons '[] '[] '[] = '[]
  SumCons ('Record name fields : cons) (as : ass) (conSpec : conSpecs) =
    SumCon name as (NamedFields fields) conSpec : SumCons cons ass conSpecs
  SumCons ('Constructor name : cons) (as : ass) (conSpec : conSpecs) =
    SumCon name as (ConstructorFields name 0 as) conSpec : SumCons cons ass conSpecs
  SumCons ('Infix _ _ _ : _) _ _ =
    PlainTypeError "Infix constructors are not supported."
  SumCons _ _ _ =
    PlainTypeError "The number of specified constructors does not match the data type."

type SumSort :: Sort -> Type -> [[Type]] -> Symbol -> [ConstructorInfo] -> [Type] -> Dd0
type family SumSort sort a ass name cons cols where
  SumSort sort a ass name cons cols =
    'Dd ('Ext0 SelAuto NoMods) a ('Comp ('TSel 'DefaultPrefix name) sort 'Nest (SumCons cons ass cols))

type SumInfo :: Type -> [[Type]] -> DatatypeInfo -> [Type] -> Dd0
type family SumInfo a as info cols where
  SumInfo a ass ('ADT _ name cons _) cols =
    SumSort ('Sum 'DefaultPrefix) a ass name cons cols
  SumInfo a _ _ _ =
    TypeError ("Sum: Not an ADT: " <> Quoted a)

type SumGen :: Type -> [Type] -> Dd0
type family SumGen a cols where
  SumGen a cols = SumInfo a (GCode a) (GDatatypeInfoOf a) cols

type SumGenDef :: Type -> [Type] -> Dd0
type family SumGenDef a cols where
  SumGenDef a cols = SumGen a cols

type SumGenAuto :: Type -> Type -> Symbol -> [ConstructorInfo] -> [[Type]] -> Dd0
type family SumGenAuto a spec name cons ass where
  SumGenAuto a spec name cons ass =
    SumSort ('Sum 'DefaultPrefix) a ass name cons (ConAutoCols spec cons ass)

type SumGenAutoDef :: Type -> Type -> Symbol -> [ConstructorInfo] -> [[Type]] -> Dd0
type family SumGenAutoDef a spec name cons ass where
  SumGenAutoDef a spec name cons ass = SumGenAuto a spec name cons ass

------------------------------------------------------------------------------------------------------------------------

type SetPathSkip :: Sel -> Sel
type family SetPathSkip sel where
  SetPathSkip ('Sel name 'Nothing) = ('Sel name ('Just 'PathSkip))
  SetPathSkip sel = sel

type SetMerge :: Dd0 -> Dd0
type family SetMerge s where
  SetMerge ('Dd ('Ext0 sel mods) a ('Comp tsel sort _ s)) =
    'Dd ('Ext0 (SetPathSkip sel) mods) a ('Comp tsel sort 'Merge (SetConNullable sort s))
  SetMerge ('Dd _ a ('Prim _)) =
    TypeError (Quoted "Merge" <> " cannot be used with primitive type " <> Quoted a)

------------------------------------------------------------------------------------------------------------------------

type PrimsGen :: Type -> Type -> [[Type]] -> DatatypeInfo -> Dd0
type family PrimsGen a spec as info where
  PrimsGen a spec '[as] ('ADT _ name '[ 'Record _ fields] _) =
    ProdSort 'Prod a as name (NamedFields fields) (AllAuto (AutoPrim spec) as)
  PrimsGen a spec ass ('ADT _ name cons _) =
    SumGenAutoDef a spec name cons ass

type Prims :: Type -> Type -> Dd0
type family Prims a spec where
  Prims (ConMeta name '[a] '[field]) spec = SetMerge (ConPrims spec name '[a] '[field])
  Prims (ConMeta name as fields) spec = ConPrims spec name as fields
  Prims a spec = PrimsGen a spec (GCode a) (GDatatypeInfoOf a)

------------------------------------------------------------------------------------------------------------------------

type Reify :: Type -> Type -> Dd0
type family Reify a spec

type instance Reify a Gen = Prims a Gen
type instance Reify a Newtypes = Prims a Newtypes

type instance Reify a (Prod cols) = ProdGen a cols
type instance Reify a (ProdAs tname cols) = Reify a (TypeName tname (Prod cols))
type instance Reify a (UidProd i b) = Reify a (Prod [i, b])

type instance Reify a (Sum cols) = SumGenDef a cols

type instance Reify (ConMeta name as fields) (Con cols) = ConFor name as fields cols
type instance Reify (ConMeta name '[a] '[field]) (Con1 cols) = SetMerge (ConFor name '[a] '[field] '[cols])

type instance Reify a (Merge spec) = SetMerge (Reify a spec)

type instance Reify a PrimAuto = PrimInfer a PrimAuto
type instance Reify a (PrimAs name) = AmendDdName name (PrimInfer a PrimAuto)
type instance Reify a (PrimUsing prim) = PrimInfer a prim
type instance Reify _ (PrimWith name a) = AmendDdName name (PrimBasic a Prim)
type instance Reify a (Param spec) = SetNoCond (Reify a spec)

type instance Reify a (Mod mod spec) = AddMod mod (Reify a spec)
type instance Reify a (Mods mods spec) = AddMods mods (Reify a spec)
type instance Reify a (ModTrans f spec) = f @@ Reify a spec
type instance Reify a (ModWith f mod spec) = AddModWith f mod (Reify a spec)

type instance Reify a (Nullable spec) = ReifyNullable 'False a spec
type instance Reify a (OrNull spec) = ReifyNullable 'True a spec
type instance Reify a (NewtypeOf spec) = PrimInfer a (NewtypeOf spec)
type instance Reify a (Array f spec) = ReifyArray a f spec

type UndetGeneral :: Type -> ErrorMessage
type UndetGeneral spec =
  "If you are calling a polymorphic function that has a constraint like " <> Quoted "ReifySqel" <> "," %
  "you probably need to use a type application to specify the spec, like " <> Quoted "Prim" <> "." %
  "If the variable is supposed to be polymorphic, you need to add " <> Quoted "ReifySqel" <>
  " to its function's context" %
  "and use the variable in the type application."

type NoSpec :: Type -> Type -> ErrorMessage
type family NoSpec a spec where
  NoSpec a spec =
    "The type (variable) " <> Quoted spec <> " specifying a column of type " <> Quoted a <> " is undetermined." %
    UndetGeneral spec

type NoReify :: Type -> Type -> ErrorMessage
type family NoReify a spec where
  NoReify a spec =
    "The spec " <> Quoted spec % " given for a column of type " <> Quoted a <> " is not supported." %
    "If you intend to use it as a custom spec, you need to define:" %
    "type instance Reify a (" <> spec <> ") = <impl>" %
    "If there is an undetermined type variable in the spec:" %
    UndetGeneral spec

type ReifyE :: Type -> Type -> Dd0
type family ReifyE a spec where
  ReifyE a spec =
    Reify a spec
    -- IfStuck spec (DelayError (NoSpec a spec))
    -- (Pure (IfStuck (Reify a spec) (DelayError (NoReify a spec)) (Pure (Reify a spec))))

type Table :: Symbol -> Type -> Type -> Dd1
type family Table name a spec where
  Table name a spec = NormalizeDd (ReifyE a (TableName name spec))

type MigrationTable :: Symbol -> Type -> Type -> Ddl
type family MigrationTable name a spec where
  MigrationTable name a spec = ToDdl (Table name a spec)

type UidTable :: Symbol -> Type -> Type -> Type -> Type -> Dd1
type family UidTable name i a si sa where
  UidTable name i a si sa = Table name (Uid i a) (UidProd (Pk si) (Merge (TypeName name sa)))

type IntTable :: Symbol -> Type -> Type -> Dd1
type IntTable name a sa = UidTable name Int64 a Prim sa

type UuidTable :: Symbol -> Type -> Type -> Dd1
type UuidTable name a sa = UidTable name UUID a Prim sa

type Query :: Type -> Type -> Dd1
type family Query a spec where
  Query a spec = NormalizeDd (ReifyE a spec)
