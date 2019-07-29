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

  devDrv = hlib.overrideCabal drv (old: rec {
    libraryHaskellDepends = old.libraryHaskellDepends ++ [
      haskellPackages.criterion
    ];
  });

  devUtils = [
    cabal-install
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

    dev = devDrv.env.overrideAttrs (old: rec {
      buildInputs = old.buildInputs ++ runtimeDeps ++ devUtils;
    });

    shell = haskellPackages.shellFor {
      packages = p: [ devDrv ];
      buildInputs = runtimeDeps ++ devUtils;
      shellHook = builtins.readFile ./prompt.sh;
    };
  }
