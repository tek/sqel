{
  description = "Hasql statement typelevel DSL";

  inputs = {
    hix.url = "git+https://git.tryp.io/tek/hix";
    exon.url = "git+https://git.tryp.io/tek/exon";
  };

  outputs = { hix, exon, ... }: hix.lib.pro ({config, lib, ...}: let
  in {
    depsFull = [exon];
    main = "sqel";
    compiler = "ghc94";

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
      paths = false;
      meta = {
        maintainer = "hackage@tryp.io";
        category = "Database";
        github = "tek/sqel";
        extra-source-files = ["changelog.md" "readme.md"];
      };
    };

    ghci.args = ["-fprint-potential-instances"];

    overrides = {jailbreak, ...}: {
      th-extras = jailbreak;
    };

    packages.sqel-core = {
      src = ./packages/core;
      versionFile = "ops/version-core.nix";

      cabal.meta.synopsis = "Hasql statement typelevel DSL";

      library.enable = true;
      library.dependencies = [
        "aeson >= 2.0 && < 2.2"
        "chronos ^>= 1.1"
        "composition ^>= 1.0"
        "containers"
        "contravariant ^>= 1.5"
        "dependent-sum-template ^>= 0.1"
        "dependent-sum-aeson-orphans ^>= 0.3"
        "exon ^>= 1.4"
        "extra ^>= 1.7"
        "first-class-families ^>= 0.8"
        "generics-sop ^>= 0.5"
        "hasql ^>= 1.6"
        "invariant ^>= 0.6"
        "path ^>= 0.9"
        "path-io ^>= 1.7"
        "prettyprinter ^>= 1.7"
        "scientific ^>= 0.3"
        "some ^>= 1.0"
        "template-haskell"
        "time"
        "transformers"
        "type-errors ^>= 0.2"
        "uuid ^>= 1.3"
        "vector ^>= 0.12"
      ];

      test = {
        enable = true;
        default-extensions = ["QualifiedDo"];
        dependencies = [
          "aeson >= 2.0 && < 2.2"
          "exon ^>= 1.4"
          "generics-sop ^>= 0.5"
          "hasql ^>= 1.6"
          "hedgehog >= 1.1 && < 1.3"
          "tasty ^>= 1.4"
          "tasty-hedgehog >= 1.3 && < 1.5"
          "transformers"
        ];
      };

      tests.sqel-integration = {
        source-dirs = "integration";
        env = "sqel-integration";
        default-extensions = ["QualifiedDo"];
        dependencies = [
          "exon ^>= 1.4"
          "hasql ^>= 1.6"
          "hedgehog >= 1.1 && < 1.3"
          "lifted-base ^>= 0.2"
          "tasty ^>= 1.4"
          "tasty-hedgehog >= 1.3 && < 1.5"
        ];
      };

    };

    packages.sqel = {
      src = ./packages/sqel;
      versionFile = "ops/version-sqel.nix";
      library = {
        enable = true;
        reexported-modules = [
          "Sqel.Crud"
          "Sqel.Statement"
        ];
      };
      cabal = {
        meta.synopsis = "Hasql statement typelevel DSL";
        dependencies = [
          config.packages.sqel-core.dep.minor
        ];
      };
    };

    envs.sqel-integration = {
      basePort = 25000;
      services.postgres = {
        enable = true;
        config = {
          name = "sqel";
          log = true;
          creds = {
            user = "sqel";
            password = "sqel";
          };
        };
      };

      env = {
        sqel_test_host = "localhost";
        sqel_test_port = config.envs.sqel-integration.hostPorts.postgres;
      };
    };

    envs.bench = {
      overrides = {minimal, ...}: { sqel = minimal; };
      haskellPackages = ["sqel" "sqel-core"];
    };

    commands.bench = {
      env = "bench";
      expose = true;
      command = let
        bench = import ./ops/bench.nix { inherit config; };
        run = file: ''
        rm -f Bench.hs
        cp ${file} Bench.hs
        command time -a -o bench.stats -f 'user %Us | wall %es | kernel %S' ghc -Wall -H64m -O +RTS -A64M -RTS Bench.hs
        cat bench.stats
        rm -f Bench.* bench.stats bench
        '';
      in ''
      ${run bench.simple}
      ${run bench.complex}
      '';
    };

  });
}
