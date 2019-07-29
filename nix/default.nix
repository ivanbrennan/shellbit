{ pkgs ? import ./nixpkgs.nix { } }:

with pkgs;

let
  drv = haskellPackages.callPackage ./.. { };

  hlib = haskell.lib;

  pkg = hlib.generateOptparseApplicativeCompletion "shellbit" (
    hlib.buildStrictly (
      (hlib.justStaticExecutables drv).overrideAttrs (old: rec {
        GHC_ENVIRONMENT = "-"; # ignore ghc environment files
        buildInputs = old.buildInputs ++ runtimeDeps; # needed for tests
      })
    )
  );

  runtimeDeps = [
    git
    nix
  ];

  devUtils = [
    cabal-install
    haskellPackages.criterion
    haskellPackages.ghcid
    hlint
  ];

in
  rec {
    minimal = pkg.overrideAttrs (old: rec {
      name = "shellbit";
    });

    full = buildEnv {
      name = "shellbit-full";
      paths = [ pkg ] ++ runtimeDeps;
    };

    dev = drv.env.overrideAttrs (old: rec {
      buildInputs = old.buildInputs ++ runtimeDeps ++ devUtils;
    });

    shell = haskellPackages.shellFor {
      packages = p: [ drv ];
      buildInputs = runtimeDeps ++ devUtils;
      shellHook = builtins.readFile ./prompt.sh;
    };
  }
