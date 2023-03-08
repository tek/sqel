{
  description = "Guided derivation for Hasql statements";

  inputs = {
    hix.url = "git+https://git.tryp.io/tek/hix";
    hls.url = "github:haskell/haskell-language-server?ref=1.9.0.0";
    exon.url = "git+https://git.tryp.io/tek/exon";
  };

  outputs = { hix, hls, exon, ... }:
  hix.lib.pro ({ config, lib, ... }: let

    overrides = {unbreak, hackage, ...}: {
      th-desugar = hackage "1.13.1" "0rfiznqlivb8zyykq49z3yz1jazy4g804h0vbmcab3fbmjfga6bz";
      singletons-base = unbreak;
    };

  in {
    packages.sqel = ./packages/sqel;
    hpack.packages = import ./ops/hpack.nix { inherit config lib; };
    depsFull = [exon];
    devGhc.compiler = "ghc925";
    inherit overrides;
    ghci = {
      args = ["-fprint-potential-instances"];
      preludePackage = "incipit-base";
      preludeModule = "IncipitBase";
      extensions = ["StandaloneKindSignatures" "OverloadedRecordDot"];
    };
    hackage.versionFile = "ops/version.nix";
    compat.enable = false;
    shell.hls.package = hls.packages.${config.system}.haskell-language-server-925;
  });
}
