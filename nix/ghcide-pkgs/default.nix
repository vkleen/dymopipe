{
  extras = hackage:
    {
      packages = {
        "apply-refact" = (((hackage.apply-refact)."0.7.0.0").revisions).default;
        "bytestring-trie" = (((hackage.bytestring-trie)."0.2.5.0").revisions).default;
        "clock" = (((hackage.clock)."0.7.2").revisions).default;
        "constrained-dynamic" = (((hackage.constrained-dynamic)."0.1.0.0").revisions).default;
        "floskell" = (((hackage.floskell)."0.10.2").revisions).default;
        "ghc-check" = (((hackage.ghc-check)."0.1.0.3").revisions).default;
        "ghc-lib-parser-ex" = (((hackage.ghc-lib-parser-ex)."8.8.2").revisions).default;
        "haddock-library" = (((hackage.haddock-library)."1.8.0").revisions).default;
        "haskell-lsp" = (((hackage.haskell-lsp)."0.21.0.0").revisions).default;
        "haskell-lsp-types" = (((hackage.haskell-lsp-types)."0.21.0.0").revisions).default;
        "haskell-src-exts" = (((hackage.haskell-src-exts)."1.21.1").revisions).default;
        "hie-bios" = (((hackage.hie-bios)."0.4.0").revisions).default;
        "hlint" = (((hackage.hlint)."2.2.8").revisions).default;
        "hoogle" = (((hackage.hoogle)."5.0.17.11").revisions).default;
        "hsimport" = (((hackage.hsimport)."0.11.0").revisions).default;
        "ilist" = (((hackage.ilist)."0.3.1.0").revisions).default;
        "lsp-test" = (((hackage.lsp-test)."0.10.2.0").revisions).default;
        "monad-dijkstra" = (((hackage.monad-dijkstra)."0.1.1.2").revisions).default;
        "semigroups" = (((hackage.semigroups)."0.18.5").revisions).default;
        "temporary" = (((hackage.temporary)."1.2.1.1").revisions).default;
        haskell-language-server = ./haskell-language-server.nix;
        cabal-helper = ./.stack-to-nix.cache.0;
        };
      };
  resolver = "lts-15.5";
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "haskell-language-server" = {
            flags = { "pedantic" = lib.mkOverride 900 true; };
            };
          };
        })
    { packages = {}; }
    ];
  }