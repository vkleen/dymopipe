{ ghcVersion ? "ghc883" }:
let
  sources = import ./sources.nix;
  haskell-nix-overlays = (import "${sources."haskell.nix"}/overlays").combined;
  pkgs = import sources.nixpkgs {
    overlays =
      [ haskell-nix-overlays
        (self: super: {
          haskell-nix = super.haskell-nix // {
            hackage = super.haskell-nix.hackage // {
              lsp-test = super.haskell-nix.hackage.lsp-test // {
                "0.10.3.0" = super.haskell-nix.hackage.lsp-test."0.10.2.0";
              };
              haskell-lsp = super.haskell-nix.hackage.haskell-lsp // {
                "0.22.0.0" = super.haskell-nix.hackage.haskell-lsp."0.21.0.0";
              };
              haskell-lsp-types = super.haskell-nix.hackage.haskell-lsp-types // {
                "0.22.0.0" = super.haskell-nix.hackage.haskell-lsp-types."0.21.0.0";
              };
            };
            # hackageSourceJSON = ./hackage-src.json;
            # stackageSourceJSON = ./stackage-src.json;
          };
        })
        (self: super: {
          "libusb-1.0" = super.libusb1;
        })
      ];
  };
  inherit (pkgs) lib;

  inherit (import sources."niv" {}) niv;

  nonReinstallablePkgs= [
    "Cabal"
     "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
    "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
    # ghcjs custom packages
    "ghcjs-prim" "ghcjs-th"
    "ghc-boot"
    "ghc" "Win32" "array" "binary" "bytestring" "containers"
    "directory" "filepath" "ghc-boot" "ghc-compact" "ghc-prim"
    # "ghci" "haskeline"
    "hpc"
    "mtl" "parsec" "process" "text" "time" "transformers"
    "unix" "xhtml"
    "stm" "terminfo"
  ];

  hie-pkgs = let
    hsPkgs = pkgs.haskell-nix.stackProject {
      src = pkgs.fetchgit {
        url = sources.haskell-language-server.repo;
        inherit (sources.haskell-language-server) rev fetchSubmodules sha256;
      };
      stackYaml = "stack-8.8.3.yaml";
      pkg-def-extras = [
        # (hackage: pkgs.lib.mapAttrs (n: v: hackage."${n}"."${v}".revisions.default) compiler-pkgs)
        (hackage: {
          lsp-test = import ./lsp-test.nix sources.lsp-test;
          haskell-lsp = import ./haskell-lsp.nix sources.haskell-lsp;
          haskell-lsp-types = import ./haskell-lsp-types.nix "${sources.haskell-lsp}/haskell-lsp-types";
          # ghcide = import ./ghcide.nix sources.ghcide;
        })
      ];
      modules = [ ({config, ...}: {
        reinstallableLibGhc = true;
        inherit nonReinstallablePkgs;
      }) ];
    };
  in {
    # ghcide = hsPkgs.ghcide.components.exes.ghcide // { packages = hsPkgs; };
    haskell-language-server = hsPkgs.haskell-language-server.components.exes.haskell-language-server;
  };

  hsPkgs = pkgs.haskell-nix.stackProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ../.; name = "dymopipe"; };
    ghc = pkgs.buildPackages.pkgs.haskell-nix.compiler."${ghcVersion}";
    pkg-def-extras = [
    ];
    modules = [ ({config, ...}: {
      reinstallableLibGhc = true;
      inherit nonReinstallablePkgs;
    }) ];
  };
in rec {
  inherit hsPkgs;
  inherit (hie-pkgs) haskell-language-server;
  shell = hsPkgs.shellFor {
    packages = ps: with ps; [
      dymopipe
    ];

    additional = ps: with ps; [
      # ghc
    ];

    withHoogle = true;
    buildInputs = (with pkgs.haskellPackages; [
      hlint stylish-haskell cabal-install niv ghcid hpack
    ]) ++ [ haskell-language-server pkgs.haskell-nix.nix-tools pkgs.stack ];

    shellHook = ''
      export NIX_GHC_LIBDIR=$(ghc --print-libdir)
    '';
  };
}
