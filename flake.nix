{
  description = "Guided derivation for Hasql statements";

  inputs = {
    hix.url = "git+https://git.tryp.io/tek/hix";
    exon.url = "git+https://git.tryp.io/tek/exon";
  };

  outputs = { hix, exon, ... }: hix.lib.pro {
    hackage.versionFile = "ops/version.nix";
    depsFull = [exon];

    overrides = {unbreak, hackage, ...}: {
      th-desugar = hackage "1.13.1" "0rfiznqlivb8zyykq49z3yz1jazy4g804h0vbmcab3fbmjfga6bz";
      singletons-base = unbreak;
    };

    cabal = {
      license = "BSD-2-Clause-Patent";
      license-file = "LICENSE";
      author = "Torsten Schmits";
      prelude = {
        enable = true;
        package = {
          name = "incipit-base";
          version = "^>= 0.5";
        };
        module = "IncipitBase";
      };
      ghc-options = ["-Wno-partial-type-signatures"];
      paths = false;
      meta = {
        maintainer = "hackage@tryp.io";
        category = "Database";
        git = "https://git.tryp.io/tek/sqel";
        homepage = "https://git.tryp.io/tek/sqel";
        bug-reports = "https://github.com/tek/sqel/issues";
      };
    };

    ghci.args = ["-fprint-potential-instances"];

    packages.sqel = {
      src = ./packages/sqel;

      cabal = {
        version = import ./ops/version.nix;
        meta.synopsis = "Guided derivation for Hasql statements";
      };

      library.enable = true;
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

      test.enable = true;
      test.dependencies = [
          "exon ^>= 1.4"
          "generics-sop ^>= 0.5"
          "hasql ^>= 1.6"
          "hedgehog ^>= 1.1"
          "microlens ^>= 0.4"
          "tasty ^>= 1.4"
          "tasty-hedgehog ^>= 1.3"
          "transformers"
        ];

    };
  };
}
