{config}:
with config.pkgs.lib;
let

  fcount = 5;
  count = 5;

  fieldsWith = num: f: concatStringsSep ", " (genList (i: "t${toString num}_${toString i} :: Text") f);

  fields = num: fieldsWith num fcount;

  statement = num: let
    t = "T${num}";
    q = "Q${num}";
  in ''
  data ${t} = ${t} { ${fields num} } deriving stock Generic
  type Table_${t} = IntTable "t${num}" ${t} Gen
  table_${t} :: Sqel Table_${t}
  table_${t} = sqel

  data ${q} = ${q} { ${fields num} } deriving stock Generic
  type Query_${q} = Query ${q} Gen
  query_${q} :: Sqel Query_${q}
  query_${q} = sqel

  stmt_${num} :: Crud ${q} (Uid Int64 ${t})
  stmt_${num} = crud query_${q} table_${t}
  '';

  header = ''
  {-# language NoImplicitPrelude, TemplateHaskell, QuasiQuotes, GHC2021, DuplicateRecordFields, DataKinds #-}
  {-# language DerivingStrategies, AllowAmbiguousTypes, ExplicitNamespaces, UnicodeSyntax, BlockArguments #-}
  {-# language OverloadedRecordDot #-}
  module Bench where
  import IncipitBase
  '';

  statements = concatStringsSep "\n" (genList (n: statement (toString n)) count);

  file = ''
  ${header}

  import Sqel
  import Sqel.Crud

  ${statements}

  main :: IO ()
  main = pure (${seqs} `seq` ())
  '';

in {
  bench = config.pkgs.writeText "Bench.hs" file;
}
