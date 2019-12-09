let
  buildDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (build dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  sysDepError = pkg:
    builtins.throw ''
      The Nixpkgs package set does not contain the package: ${pkg} (system dependency).
      
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      '';
  pkgConfDepError = pkg:
    builtins.throw ''
      The pkg-conf packages does not contain the package: ${pkg} (pkg-conf dependency).
      
      You may need to augment the pkg-conf package mapping in haskell.nix so that it can be found.
      '';
  exeDepError = pkg:
    builtins.throw ''
      The local executable components do not include the component: ${pkg} (executable dependency).
      '';
  legacyExeDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (executable dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  buildToolDepError = pkg:
    builtins.throw ''
      Neither the Haskell package set or the Nixpkgs package set contain the package: ${pkg} (build tool dependency).
      
      If this is a system dependency:
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      
      If this is a Haskell dependency:
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
in { system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "2.4";
      identifier = { name = "hie-bios"; version = "0.3.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Matthew Pickering <matthewtpickering@gmail.com>";
      author = "Matthew Pickering <matthewtpickering@gmail.com>";
      homepage = "https://github.com/mpickering/hie-bios";
      url = "";
      synopsis = "Set up a GHC API session";
      description = "Set up a GHC API session and obtain flags required to compile a source file";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = "";
      dataFiles = [];
      extraSrcFiles = [
        "ChangeLog"
        "wrappers/cabal"
        "wrappers/cabal.hs"
        "tests/configs/*.yaml"
        "tests/projects/**/*.hs"
        "tests/projects/**/*.cabal"
        "tests/projects/**/*.md"
        "tests/projects/**/*.project"
        "tests/projects/**/*.yaml"
        "tests/projects/**/*.sh"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."base16-bytestring" or (buildDepError "base16-bytestring"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."deepseq" or (buildDepError "deepseq"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."cryptohash-sha1" or (buildDepError "cryptohash-sha1"))
          (hsPkgs."directory" or (buildDepError "directory"))
          (hsPkgs."filepath" or (buildDepError "filepath"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."extra" or (buildDepError "extra"))
          (hsPkgs."process" or (buildDepError "process"))
          (hsPkgs."ghc" or (buildDepError "ghc"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."temporary" or (buildDepError "temporary"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."unix-compat" or (buildDepError "unix-compat"))
          (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
          (hsPkgs."vector" or (buildDepError "vector"))
          (hsPkgs."yaml" or (buildDepError "yaml"))
          (hsPkgs."hslogger" or (buildDepError "hslogger"))
          (hsPkgs."file-embed" or (buildDepError "file-embed"))
          (hsPkgs."conduit" or (buildDepError "conduit"))
          (hsPkgs."conduit-extra" or (buildDepError "conduit-extra"))
          ];
        buildable = true;
        modules = [
          "Paths_hie_bios"
          "HIE/Bios"
          "HIE/Bios/Config"
          "HIE/Bios/Cradle"
          "HIE/Bios/Environment"
          "HIE/Bios/Internal/Debug"
          "HIE/Bios/Flags"
          "HIE/Bios/Types"
          "HIE/Bios/Internal/Log"
          "HIE/Bios/Ghc/Api"
          "HIE/Bios/Ghc/Check"
          "HIE/Bios/Ghc/Doc"
          "HIE/Bios/Ghc/Gap"
          "HIE/Bios/Ghc/Load"
          "HIE/Bios/Ghc/Logger"
          "HIE/Bios/Wrappers"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "hie-bios" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            (hsPkgs."ghc" or (buildDepError "ghc"))
            (hsPkgs."hie-bios" or (buildDepError "hie-bios"))
            ];
          buildable = true;
          modules = [ "Paths_hie_bios" ];
          hsSourceDirs = [ "exe" ];
          mainPath = [ "Main.hs" ];
          };
        };
      tests = {
        "parser-tests" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."tasty" or (buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (buildDepError "tasty-hunit"))
            (hsPkgs."hie-bios" or (buildDepError "hie-bios"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            ];
          buildable = true;
          hsSourceDirs = [ "tests/" ];
          mainPath = [ "ParserTests.hs" ];
          };
        "bios-tests" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."tasty" or (buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (buildDepError "tasty-hunit"))
            (hsPkgs."hie-bios" or (buildDepError "hie-bios"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."ghc" or (buildDepError "ghc"))
            ];
          buildable = true;
          hsSourceDirs = [ "tests/" ];
          mainPath = [ "BiosTests.hs" ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/mpickering/hie-bios.git";
      rev = "32c70fe232cfa6186c7e333205ec4ba103b8ad19";
      sha256 = "1w3jam3rzkjr432s6lr804w4c44hx10kfnsgscm4ikn61bsnpakm";
      });
    }