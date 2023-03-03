{ config, lib, ... }:
with builtins;
with lib;
let

  mergeAttr = a: b:
  if isAttrs a
  then merge a b
  else if isList a
  then a ++ b
  else b;

  merge = l: r:
  let
    f = name:
    if hasAttr name l && hasAttr name r
    then mergeAttr l.${name} r.${name}
    else l.${name} or r.${name};
  in genAttrs (concatMap attrNames [l r]) f;

  paths = name: {
    when = {
      condition = false;
      generated-other-modules = ["Paths_${replaceStrings ["-"] ["_"] name}"];
    };
  };

  meta = {
    version = import ./version.nix;
    license = "BSD-2-Clause-Patent";
    license-file = "LICENSE";
    author = "Torsten Schmits";
    maintainer = "hackage@tryp.io";
    copyright = "2023 Torsten Schmits";
    category = "Database";
    build-type = "Simple";
    git = "https://git.tryp.io/tek/sqel";
    homepage = "https://git.tryp.io/tek/sqel";
    bug-reports = "https://github.com/tek/sqel/issues";
  };

  options.ghc-options = [
    "-Wall"
    "-Wredundant-constraints"
    "-Wincomplete-uni-patterns"
    "-Wmissing-deriving-strategies"
    "-Widentities"
    "-Wunused-packages"
  ];

  dependencies = [
    { name = "base"; version = ">= 4.12 && < 5"; mixin = "hiding (Prelude)"; }
    { name = "incipit-base"; version = "^>= 0.5"; mixin = ["(IncipitBase as Prelude)" "hiding (IncipitBase)"]; }
  ];

  project = name: doc: merge (meta // { library = paths name; } // options) {
    inherit name;
    description = "See https://hackage.haskell.org/package/${name}/docs/${doc}.html";
    library = {
      source-dirs = "lib";
      inherit dependencies;
    };
    default-extensions = config.ghci.extensions;
  };

  exe = pkg: dir: merge (paths pkg // {
    main = "Main.hs";
    source-dirs = dir;
    dependencies = dependencies ++ [pkg];
    ghc-options = [
      "-threaded"
      "-rtsopts"
      "-with-rtsopts=-N"
    ];
  });

in {

  sqel = merge (project "sqel" "Sqel") {
    synopsis = "Guided derivation for Hasql statements";
    library.dependencies = [
      "aeson ^>= 2.0"
      "chronos ^>= 1.1"
      "composition ^>= 1.0"
      "containers"
      "contravariant ^>= 1.5"
      "exon ^>= 1.4"
      "extra ^>= 1.7"
      "first-class-families ^>= 0.8"
      "generic-lens ^>= 2.2"
      "generics-sop ^>= 0.5"
      "hasql ^>= 1.6"
      "invariant ^>= 0.6"
      "microlens ^>= 0.4"
      "path ^>= 0.9"
      "path-io ^>= 1.7"
      "prettyprinter ^>= 1.7"
      "scientific ^>= 0.3"
      "singletons ^>= 3"
      "singletons-base ^>= 3.1"
      "some ^>= 1.0"
      "template-haskell"
      "time"
      "transformers"
      "type-errors ^>= 0.2"
      "uuid ^>= 1.3"
      "vector ^>= 0.12"
    ];

    tests.sqel-unit = exe "sqel" "test" {
      dependencies = [
        "exon ^>= 1.4"
        "generics-sop ^>= 0.5"
        "hedgehog ^>= 1.1"
        "microlens ^>= 0.4"
        "tasty ^>= 1.4"
        "tasty-hedgehog ^>= 1.3"
      ];
    };

  };


}
