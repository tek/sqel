module Sqel.Dsl (
  Prim,
  PrimAs,
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
import Fcf (Eval, Exp, Pure, type (@@))
import Generics.SOP.GGP (GCode, GDatatypeInfoOf)
import qualified Generics.SOP.Type.Metadata as SOP
import Generics.SOP.Type.Metadata (ConstructorInfo (Constructor, Infix, Record), DatatypeInfo (ADT))
import Prelude hiding (Enum, Mod)
import Type.Errors (DelayError, ErrorMessage, IfStuck)

import Sqel.Data.Dd (
  ConCol,
  Dd,
  Dd0,
  DdK (Dd),
  Ext0 (Ext0),
  Inc (Merge, Nest),
  PrimType (Cond, NoCond),
  Sort (Con, Prod, Sum),
  StructWith (Comp, Prim),
  )
import Sqel.Data.Mods (NoMods)
import qualified Sqel.Data.Mods.Array as Mods (Array)
import qualified Sqel.Data.Mods.Enum as Mods (Enum)
import qualified Sqel.Data.Mods.Ignore as Mods
import qualified Sqel.Data.Mods.Json as Mods
import Sqel.Data.Mods.MigrationDefault (MigrationDefault)
import Sqel.Data.Mods.MigrationDelete (MigrationDelete)
import Sqel.Data.Mods.MigrationRename (MigrationRename)
import Sqel.Data.Mods.MigrationRenameIndex (MigrationRenameIndex)
import Sqel.Data.Mods.MigrationRenameType (MigrationRenameType)
import Sqel.Data.Mods.Name (SetPrimName)
import qualified Sqel.Data.Mods.Newtype as Mods (Newtype)
import qualified Sqel.Data.Mods.Nullable as Mods (Nullable)
import qualified Sqel.Data.Mods.PrimaryKey as Mods (PrimaryKey)
import qualified Sqel.Data.Mods.Unique as Mods (Unique)
import Sqel.Data.Name (AmendName, NamePrefix (DefaultPrefix, NamePrefix))
import Sqel.Data.Sel (Path (PathSkip), Sel (Sel), SelAuto, TSel (TSel), TSelWithPrefix)
import Sqel.Data.Uid (Uid)
import Sqel.Dd (SetDdName)
import Sqel.Dsl.Comp
import Sqel.Dsl.Error (TypeNamePrimError)
import Sqel.Dsl.Fields (Field (FieldNum), NamedFields, ReifyFieldNames)
import Sqel.Dsl.Mod (AddMod, AddModWith, AddMods, ApplyMod, Mod, ModTrans, ModWith, Mods)
import Sqel.Dsl.Prim (AllAuto, Param, Prim, PrimAs, PrimAuto, PrimWith)
import Sqel.Migration.Ddl (Ddl, ToDdl)
import Sqel.Normalize (NormalizeQueryDd)
import Sqel.SOP.Error (Quoted, QuotedType)

type FromGenF k = DatatypeInfo -> [[Type]] -> Exp k

type FromGen :: ∀ k . FromGenF k -> Type -> k
type family FromGen f a where
  FromGen f a = f (GDatatypeInfoOf a) @@ GCode a

------------------------------------------------------------------------------------------------------------------------

type SetType :: Type -> Dd0 -> Dd0
type family SetType a s where
  SetType a ('Dd ext _ s) = 'Dd ext a s

data WrapType :: (Type -> Type) -> Dd0 -> Exp Dd0
type instance Eval (WrapType f ('Dd ext a s)) = 'Dd ext (f a) s

data ApplyTypeName :: Symbol -> Dd0 -> Exp Dd0
type instance Eval (ApplyTypeName name ('Dd ext a ('Comp ('TSel prefix _) c i s))) =
  'Dd ext a ('Comp ('TSel prefix name) c i s)
type instance Eval (ApplyTypeName name ('Dd ext a ('Prim _))) =
  TypeNamePrimError name ext a

data ApplyTableName :: Symbol -> Dd0 -> Exp Dd0
type instance Eval (ApplyTableName name ('Dd ext a ('Comp ('TSel prefix _) c i s))) =
  'Dd ext a ('Comp ('TSel prefix name) c i s)
type instance Eval (ApplyTableName name ('Dd ext a ('Prim prim))) =
  AmendDdName name ('Dd ext a ('Prim prim))

data ApplyTypePrefix :: Symbol -> Dd0 -> Exp Dd0
type instance Eval (ApplyTypePrefix prefix ('Dd ext a ('Comp tsel c i s))) =
  'Dd ext a ('Comp (TSelWithPrefix prefix tsel) c i s)

data ApplyIndexPrefix :: Symbol -> Dd0 -> Exp Dd0
type instance Eval (ApplyIndexPrefix prefix ('Dd ext a ('Comp tsel ('Sum _) i s))) =
  'Dd ext a ('Comp tsel ('Sum ('NamePrefix prefix)) i s)

data ApplyName :: Symbol -> Dd0 -> Exp Dd0
type instance Eval (ApplyName name s) =
  SetDdName name s

------------------------------------------------------------------------------------------------------------------------

type ApplyNullable :: Bool -> Dd0 -> Dd0
type family ApplyNullable guard s where
  ApplyNullable guard s =
    AddMod (Mods.Nullable guard) (WrapType Maybe @@ s)

type ReifyNullable :: Bool -> Type -> Type -> Dd0
type family ReifyNullable guard a spec where
  ReifyNullable guard (Maybe a) spec =
    ApplyNullable guard (ReifyE a spec)
  ReifyNullable _ a _ =
    TypeError ("A column was declared as nullable, but its type is not " <> Quoted Maybe <> ":" % Quoted a)

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
    AddMod (Mods.Array f) (WrapType f @@ s)

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

type Newtype :: Type -> Type
data Newtype spec

-- TODO Enum is a primitive mod, it cannot be applied to specs other than Prim.
-- maybe it should not take an arg, but for something like @Maybe (Vector E)@ it would be nice to have a way to use
-- inference for Maybe and Vector, but specify Enum for E.
-- could be something like @Auto Enum@.
type Enum :: Type -> Type
type Enum = Mods [SetPrimName "text", Mods.Enum]

type TypeName :: Symbol -> Type -> Type
type TypeName p = ModTrans (ApplyTypeName p)

type TableName :: Symbol -> Type -> Type
type TableName p = ModTrans (ApplyTableName p)

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

type Json :: Type
type Json = Mod Mods.Json Prim

type Jsonb :: Type
type Jsonb = Mod Mods.Jsonb Prim

------------------------------------------------------------------------------------------------------------------------

type AmendDdName :: Symbol -> Dd0 -> Dd0
type family AmendDdName name s where
  AmendDdName name ('Dd ('Ext0 ('Sel old path) mods) a s) =
    'Dd ('Ext0 ('Sel (AmendName name old) path) mods) a s

type TypeSel :: TSel -> Dd0 -> Dd0
type family TypeSel tsel s where
  TypeSel tsel ('Dd ext a ('Comp _ c i sub)) =
    'Dd ext a ('Comp tsel c i sub)

type SetTypeName :: Symbol -> Dd0 -> Dd0
type family SetTypeName name s where
  SetTypeName name s = TypeSel ('TSel 'DefaultPrefix name) s

type SetNoCond :: Dd0 -> Dd0
type family SetNoCond s where
  SetNoCond ('Dd ext a ('Prim _)) = 'Dd ext a ('Prim 'NoCond)
  SetNoCond ('Dd _ a ('Comp _ _ _ _)) =
    TypeError (
      "Cannot mark composite type " <> Quoted a <> " as a " <> Quoted Param <> "." %
      "This modifier means that the query field will be skipped in condition clauses like" %
      Quoted "where" <> ", which only applies to primitive columns."
    )

type PrimBasic :: Type -> Dd0
type family PrimBasic a where
  PrimBasic a = 'Dd ('Ext0 SelAuto NoMods) a ('Prim 'Cond)

type PrimExplicit :: Symbol -> Type -> Dd0
type family PrimExplicit name a where
  PrimExplicit name a = AmendDdName name (PrimBasic a)

type PrimInfer :: Type -> Dd0
type family PrimInfer a where
  PrimInfer (Maybe a) = ApplyNullable 'False (PrimInfer a)
  PrimInfer [a] = ApplyArray [] (PrimInfer a)
  PrimInfer (NonEmpty a) = ApplyArray NonEmpty (PrimInfer a)
  PrimInfer (Seq a) = ApplyMod (Array Seq) (PrimInfer a)
  PrimInfer (Vector a) = ApplyMod (Array Vector) (PrimInfer a)
  PrimInfer (Set a) = ApplyMod (Array Set) (PrimInfer a)
  PrimInfer a = PrimBasic a

------------------------------------------------------------------------------------------------------------------------

type ProdCol :: Symbol -> Type -> Type -> Dd0
type family ProdCol name a spec where
  ProdCol name a spec = AmendDdName name (Reify a spec)

-- TODO include specifics in count mismatch error
type ProdCols :: [Symbol] -> [Type] -> [Type] -> [Dd0]
type family ProdCols fields as cols where
  ProdCols '[] '[] '[] = '[]
  ProdCols (name : fields) (a : as) (col : cols) = ProdCol name a col : ProdCols fields as cols
  ProdCols _ _ _ = TypeError (ToErrorMessage "The number of specified columns does not match the data type.")

type ProdSort :: Sort -> Type -> [Type] -> Symbol -> [Field] -> [Type] -> Dd0
type family ProdSort sort a as name fields cols where
  ProdSort sort a as name fields cols =
    'Dd ('Ext0 SelAuto NoMods) a ('Comp ('TSel 'DefaultPrefix name) sort 'Nest (ProdCols (ReifyFieldNames name fields) as cols))

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

type ConAutoCols :: [ConstructorInfo] -> [[Type]] -> [Type]
type family ConAutoCols cons ass where
  ConAutoCols '[] _ = '[]
  ConAutoCols ('Constructor _ : cons) ('[_] : ass) = Gen : ConAutoCols cons ass
  ConAutoCols ('Record _ _ : cons) ('[_] : ass) = Gen : ConAutoCols cons ass
  ConAutoCols ('Constructor _ : cons) (_ : ass) = Gen : ConAutoCols cons ass
  ConAutoCols ('Record _ _ : cons) (_ : ass) = Gen : ConAutoCols cons ass

type ConFor :: Symbol -> [Type] -> [Field] -> [Type] -> Dd0
type family ConFor name as fields cols where
  ConFor name as fields cols =
    ProdSort 'Con (ConCol as) as name fields cols

type ConPrims :: Symbol -> [Type] -> [Field] -> Dd0
type family ConPrims name as fields where
  ConPrims name as fields =
    ProdSort 'Con (ConCol as) as name fields (AllAuto as)

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
    TypeError (ToErrorMessage ("Infix constructors are not supported."))
  SumCons _ _ _ =
    TypeError (ToErrorMessage "The number of specified constructors does not match the data type.")

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

type SumGenAuto :: Type -> Symbol -> [ConstructorInfo] -> [[Type]] -> Dd0
type family SumGenAuto a name cons ass where
  SumGenAuto a name cons ass =
    SumSort ('Sum 'DefaultPrefix) a ass name cons (ConAutoCols cons ass)

data SumGenAutoFor :: Type -> FromGenF Dd0
type instance Eval (SumGenAutoFor a ('ADT _ name cons _) ass) =
  SumGenAuto a name cons ass

type SumGenAutoDef :: Type -> Symbol -> [ConstructorInfo] -> [[Type]] -> Dd0
type family SumGenAutoDef a name cons ass where
  SumGenAutoDef a name cons ass = SumGenAuto a name cons ass

------------------------------------------------------------------------------------------------------------------------

type SetPathSkip :: Sel -> Sel
type family SetPathSkip sel where
  SetPathSkip ('Sel name 'Nothing) = ('Sel name ('Just 'PathSkip))
  SetPathSkip sel = sel

type SetMerge :: Dd0 -> Dd0
type family SetMerge s where
  SetMerge ('Dd ('Ext0 sel mods) a ('Comp tsel c _ s)) =
    'Dd ('Ext0 (SetPathSkip sel) mods) a ('Comp tsel c 'Merge s)
  SetMerge ('Dd _ a ('Prim _)) =
    TypeError (Quoted "Merge" <> " cannot be used with primitive type " <> QuotedType a)

------------------------------------------------------------------------------------------------------------------------

type PrimsGen :: Type -> [[Type]] -> DatatypeInfo -> Dd0
type family PrimsGen a as info where
  PrimsGen a '[as] ('ADT _ name '[ 'Record _ fields] _) =
    ProdSort 'Prod a as name (NamedFields fields) (AllAuto as)
  PrimsGen a ass ('ADT _ name cons _) =
    SumGenAutoDef a name cons ass

type Prims :: Type -> Dd0
type family Prims a where
  Prims (ConMeta name '[a] '[field]) = SetMerge (ConPrims name '[a] '[field])
  Prims (ConMeta name as fields) = ConPrims name as fields
  Prims a = PrimsGen a (GCode a) (GDatatypeInfoOf a)

------------------------------------------------------------------------------------------------------------------------

type Reify :: Type -> Type -> Dd0
type family Reify a spec

type instance Reify a Gen = Prims a

type instance Reify a (Prod cols) = ProdGen a cols
type instance Reify a (ProdAs tname cols) = Reify a (TypeName tname (Prod cols))
type instance Reify a (UidProd i b) = Reify a (Prod [i, b])

type instance Reify a (Sum cols) = SumGenDef a cols

type instance Reify (ConMeta name as fields) (Con cols) = ConFor name as fields cols
type instance Reify (ConMeta name '[a] '[field]) (Con1 cols) = SetMerge (ConFor name '[a] '[field] '[cols])

type instance Reify a (Merge spec) = SetMerge (Reify a spec)

type instance Reify a PrimAuto = PrimInfer a
type instance Reify a (PrimAs name) = AmendDdName name (PrimInfer a)
type instance Reify _ (PrimWith name a) = PrimExplicit name a
type instance Reify a (Param spec) = SetNoCond (Reify a spec)

type instance Reify a (Mod mod spec) = AddMod mod (Reify a spec)
type instance Reify a (Mods mods spec) = AddMods mods (Reify a spec)
type instance Reify a (ModTrans f spec) = f @@ Reify a spec
type instance Reify a (ModWith f mod spec) = AddModWith f mod (Reify a spec)

type instance Reify a (Nullable spec) = ReifyNullable 'False a spec
type instance Reify a (OrNull spec) = ReifyNullable 'True a spec
type instance Reify a (Newtype spec) = FromGen (ReifyNewtype a spec) a
type instance Reify a (Array f spec) = ReifyArray a f spec

type NoReify :: Type -> Type -> ErrorMessage
type family NoReify a spec where
  NoReify a spec =
    "The spec " <> Quoted spec % "given for a column of type " <> Quoted a <> " is not supported." %
    "If you intend to use it as a custom spec, you need to define:" %
    "type instance Reify a " <> spec <> " = <impl>"

type ReifyE :: Type -> Type -> Dd0
type family ReifyE a spec where
  ReifyE a spec = IfStuck (Reify a spec) (DelayError (NoReify a spec)) (Pure (Reify a spec))

type Table :: Symbol -> Type -> Type -> Dd
type family Table name a spec where
  Table name a spec = NormalizeQueryDd (ReifyE a (TableName name spec))

type MigrationTable :: Symbol -> Type -> Type -> Ddl
type family MigrationTable name a spec where
  MigrationTable name a spec = ToDdl (Table name a spec)

type UidTable :: Symbol -> Type -> Type -> Type -> Type -> Dd
type family UidTable name i a si sa where
  UidTable name i a si sa = Table name (Uid i a) (UidProd (Pk si) (Merge (TypeName name sa)))

type IntTable :: Symbol -> Type -> Type -> Dd
type IntTable name a sa = UidTable name Int64 a Prim sa

type UuidTable :: Symbol -> Type -> Type -> Dd
type UuidTable name a sa = UidTable name UUID a Prim sa

type Query :: Type -> Type -> Dd
type family Query a spec where
  Query a spec = NormalizeQueryDd (ReifyE a spec)
