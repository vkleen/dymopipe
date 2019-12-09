{ ghcVersion ? "ghc865" }:
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
  ghcide-pkgs = let
    stack-pkgs = ({
        extras = hackage:
          {
            packages = {
              "haskell-lsp" = (((hackage.haskell-lsp)."0.18.0.0").revisions).default;
              "haskell-lsp-types" = (((hackage.haskell-lsp-types)."0.18.0.0").revisions).default;
              "lsp-test" = (((hackage.lsp-test)."0.8.2.0").revisions).default;
              ghcide = ./ghcide.nix;
              hie-bios = ./hie-bios.nix;
            };
          };
        resolver = "nightly-2019-09-16";
        modules = [ ({ lib, ... }: { packages = {}; }) { packages = {}; } ];
    });
    pkgSet = pkgs.haskell-nix.mkStackPkgSet {
      stack-pkgs = stack-pkgs;
      modules = [({config, ...}: {
        # compiler.version = pkgs.lib.mkForce ghc.package.version;
        reinstallableLibGhc = true;
        ghc.package = pkgs.buildPackages.pkgs.haskell-nix.compiler."${ghcVersion}";
        packages.ghc.flags.ghci = pkgs.lib.mkForce true;
        packages.ghci.flags.ghci = pkgs.lib.mkForce true;
        packages.ghcide.configureFlags = [ "--enable-executable-dynamic" ];
      })];
    };
    inherit (pkgSet.config) hsPkgs;
  in {
    hie-bios = hsPkgs.hie-bios.components.exes.hie-bios;
    ghcide = hsPkgs.ghcide.components.exes.ghcide // { packages = hsPkgs; };
  };

  hsPkgs = pkgs.haskell-nix.cabalProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ../.; };
    ghc = pkgs.buildPackages.pkgs.haskell-nix.compiler."${ghcVersion}";
  };
in rec {
  inherit hsPkgs;
  inherit (ghcide-pkgs) ghcide hie-bios;
  shell = hsPkgs.shellFor {
    packages = ps: with ps; [
      dymopipe
    ];

    additional = ps: with ps; [
    ];

    withHoogle = true;
    buildInputs = (with pkgs.haskellPackages; [
      hlint stylish-haskell cabal-install niv hpack
    ]) ++ [ ghcide hie-bios pkgs.haskell-nix.nix-tools ];

    shellHook = ''
      export NIX_GHC_LIBDIR=$(ghc --print-libdir)
    '';
  };
}
