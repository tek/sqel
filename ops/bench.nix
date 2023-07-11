{config}:
with config.pkgs.lib;
let

  fcount = 10;
  count = 5;

  fieldsWith = num: f: concatStringsSep ", " (genList (i: "t${toString num}_${toString i} :: Text") f);

  fields = num: fieldsWith num fcount;

  tableComplex = ''

  newtype IntNt = IntNt Int64 deriving (Eq, Show, Generic)

  data A a =
    A {
      a1 :: a,
      a2 :: Int64,
      a3 :: Maybe Int64,
      a4 :: Maybe Bool
    }
    deriving stock (Generic)

  data B =
    B {
      b1 :: IntNt,
      b2 :: IntNt,
      b3 :: IntNt,
      b4 :: IntNt,
      b5 :: IntNt
    }
    deriving stock (Generic)

  data S1 =
    S1 {
      time :: Int,
      b :: B
    }
    deriving stock (Generic)

  data S2 =
    S2 {
      s11 :: Int,
      s12 :: Text
    }
    deriving stock (Generic)

  data S =
    SC1 { sc1 :: S1 }
    |
    SC2 { sc2 :: S2 }
    deriving stock (Generic)

  type Type_A d sd = Prod [Merge sd, Prim, Prim, Prim]

  type Type_B = Prod [Newtype Prim, Newtype Prim, Newtype Prim, Newtype Prim, Newtype Prim]

  type Table_C name i d sd = UidTable name i (A d) Prim (Type_A d sd)

  type Table_C_Int name d sd = Table_C name Int d sd

  type Type_S1 = Prod [Prim, Merge Type_B]

  type Type_S2 = Gen

  type Type_S = Sum [Con1 Type_S1, Con1 Type_S2]

  type Table_S = Table_C_Int "s" S Type_S

  table_S :: Sqel Table_S
  table_S = sqel
  '';

  statement = num: let
    t = "T${num}";
  in ''
  data ${t} = ${t} { ${fields num} } deriving stock Generic
  type Table_${t} = IntTable "t${num}" ${t} Gen
  table_${t} :: Sqel Table_${t}
  table_${t} = sqel
  '';

  dump = false;

  header = ''
  ${if dump then "{-# options -ddump-simpl -dsuppress-all #-}" else ""}
  {-# language NoImplicitPrelude, TemplateHaskell, QuasiQuotes, GHC2021, DuplicateRecordFields, DataKinds #-}
  {-# language DerivingStrategies, AllowAmbiguousTypes, ExplicitNamespaces, UnicodeSyntax, BlockArguments #-}
  {-# language OverloadedRecordDot, NoFieldSelectors #-}
  module Bench where
  import IncipitBase hiding (Enum)
  '';

  tableSimple = let
    decls = concatStringsSep "\n" (genList (n: statement (toString n)) count);
  in ''
  ${decls}
  '';

  fileWith = s: ''
  ${header}

  import Sqel

  ${s}

  main :: IO ()
  main = unit
  '';

  simple = fileWith tableSimple;

  complex = fileWith tableComplex;

in {
  simple = config.pkgs.writeText "Bench.hs" simple;
  complex = config.pkgs.writeText "Bench.hs" complex;
}
