{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs;
    flake-utils.url = github:numtide/flake-utils;
    
    generic-override = {
      url = github:estatico/generic-override;
      flake = false;
    };
    hexquote = {
      url = github:Philonous/hexquote;
      flake = false;
    };
    type-level-sets = {
      url = github:dorchard/type-level-sets;
      flake = false;
    };
    relude = {
      url = github:kowainik/relude;
      flake = false;
    };
    vinyl = {
      url = github:VinylRecords/Vinyl;
      flake = false;
    };
    singletons = {
      url = github:goldfirere/singletons;
      flake = false;
    };
  };
  outputs = inputs@{self, ...}: inputs.flake-utils.lib.eachDefaultSystem (system: let
    inherit (inputs.nixpkgs) lib;

    pkgs = import "${inputs.nixpkgs}/pkgs/top-level" {
      localSystem = { inherit system; };
      overlays = [
        (final: prev: {
          "libusb-1.0" = prev.libusb1;
        })
        (final: prev: {
          haskell = prev.haskell // {
            packageOverrides = hfinal: hprev: {
              generic-override = hfinal.callCabal2nixWithOptions "generic-override" "${inputs.generic-override}" "--subpath generic-override" {};
              hexquote = hfinal.callCabal2nix "hexquote" "${inputs.hexquote}" {};
              type-level-sets = hfinal.callCabal2nix "type-level-sets" "${inputs.type-level-sets}" {};
              vinyl = hfinal.callCabal2nix "vinyl" "${inputs.vinyl}" {};
              bindings-libusb = hprev.bindings-libusb.override { libusb = final.libusb1; };
              relude = hfinal.callCabal2nixWithOptions "relude" "${inputs.relude}" "--jailbreak" {};
              system-fileio = final.haskell.lib.dontCheck hprev.system-fileio;
              singletons = hfinal.callCabal2nixWithOptions "singletons" "${inputs.singletons}" "--subpath singletons" {};
              singletons-th = hfinal.callCabal2nixWithOptions "singletons-th" "${inputs.singletons}" "--subpath singletons-th" {};
              singletons-base = hfinal.callCabal2nixWithOptions "singletons-base" "${inputs.singletons}" "--subpath singletons-base" {};
            };
          };
        })
      ];
      config = {
        allowBroken = true;
      };
    };
    
    ghcVersion = "924";

    dymopipe = pkgs.haskell.packages."ghc${ghcVersion}".callCabal2nix "dymopipe" "${self}" {};
  in {
    packages = {
      inherit dymopipe;
    };

    defaultPackage = self.packages.${system}.dymopipe;
  });
}
