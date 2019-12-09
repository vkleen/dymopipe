let
  sources = import ./sources.nix;
  pkgs = import sources."nixpkgs" {};
  haskell = import sources."haskell.nix" { inherit pkgs; };
in
  haskell.nix-tools
