{ ghcVersion ? "ghc882" }:
let
  sources = import ./sources.nix;
  pkgs = import sources.nixpkgs {
    overlays =
      import "${sources."haskell.nix"}/overlays" ++
      [ (self: super: {
        "libusb-1.0" = super.libusb1;
      }) ];
  };
  inherit (pkgs) lib;

  inherit (import sources."niv" {}) niv;

  compiler-pkgs = {
    "binary" = "0.8.7.0";
    "ghc-prim" = "0.5.3";
    "haskeline" = "0.7.5.0";
    "stm" = "2.5.0.0";
    "unix" = "2.7.2.2";
    "mtl" = "2.2.2";
    "rts" = "1.0";
    "deepseq" = "1.4.4.0";
    "ghc-compact" = "0.1.0.0";
    "parsec" = "3.1.14.0";
    "directory" = "1.3.6.0";
    "template-haskell" = "2.15.0.0";
    "containers" = "0.6.2.1";
    "bytestring" = "0.10.10.0";
    "xhtml" = "3000.2.2.1";
    "text" = "1.2.4.0";
    "Cabal" = "3.0.1.0";
    "time" = "1.9.3";
    "terminfo" = "0.4.1.4";
    "transformers" = "0.5.6.2";
    "hpc" = "0.6.0.3";
    "filepath" = "1.4.2.1";
    "process" = "1.6.8.0";
    "pretty" = "1.1.3.6";
    "array" = "0.5.4.0";
    "Win32" = "2.6.1.0";
    "integer-gmp" = "1.0.2.0";
    "alex" = "3.2.5";
    "happy" = "1.19.12";
  };
  hie-pkgs = let
    hsPkgs = pkgs.haskell-nix.stackProject {
      src = sources.haskell-language-server;
      stackYaml = "stack-8.8.3.yaml";
      stack-sha256 = "1gnip814nhkdpdrr3zamih4134k6g80clx788gsg00sb7n34a3nv";
      materialized = ./ghcide-pkgs;
      pkg-def-extras = [
        (hackage: pkgs.lib.mapAttrs (n: v: hackage."${n}"."${v}".revisions.default) compiler-pkgs)
        (hackage: {
          ghcide = import ./ghcide.nix sources.ghcide;
        })
      ];
      modules = [ ({config, ...}: {
        reinstallableLibGhc = true;
        packages.ghc.flags.ghci = pkgs.lib.mkForce true;
        packages.ghci.flags.ghci = pkgs.lib.mkForce true;
        packages.ghcide.configureFlags = [ "--enable-executable-dynamic" ];
        packages.haskell-lsp.components.library.doHaddock = pkgs.lib.mkForce false;
        packages.ghcide.components.library.doHaddock = pkgs.lib.mkForce false;
      }) ];
    };
  in {
    ghcide = hsPkgs.ghcide.components.exes.ghcide // { packages = hsPkgs; };
#    haskell-language-server = hsPkgs.haskell-language-server.components.exes.haskell-language-server;
  };

  hsPkgs = pkgs.haskell-nix.stackProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ../.; name = "dymopipe"; };
    ghc = pkgs.buildPackages.pkgs.haskell-nix.compiler."${ghcVersion}";
    modules = [ ({config, ...}: {
      reinstallableLibGhc = true;
    }) ];
  };
in rec {
  inherit hsPkgs;
  inherit (hie-pkgs) ghcide;
  shell = hsPkgs.shellFor {
    packages = ps: with ps; [
      dymopipe
    ];

    additional = ps: with ps; [
    ];

    withHoogle = true;
    buildInputs = (with pkgs.haskellPackages; [
      hlint stylish-haskell cabal-install niv hpack ghcid
    ]) ++ [ ghcide pkgs.haskell-nix.nix-tools pkgs.stack ];

    shellHook = ''
      export NIX_GHC_LIBDIR=$(ghc --print-libdir)
    '';
  };
}
