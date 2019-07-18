{ pkgs ? import ./nixpkgs.nix { } }:

with pkgs;

let
  drv = haskellPackages.callPackage ./.. { };

  hlib = haskell.lib;

  pkg = hlib.generateOptparseApplicativeCompletion "nix-shell-bit" (
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
    hlint
  ];

in
  rec {
    full = buildEnv {
      name = "nix-shell-bit";
      paths = [ pkg ] ++ runtimeDeps;
    };

    minimal = pkg.overrideAttrs (old: rec {
      name = "nix-shell-bit-minimal";
    });

    dev = drv.env.overrideAttrs (old: rec {
      buildInputs = old.buildInputs ++ runtimeDeps ++ devUtils;
    });

    shell = haskellPackages.shellFor {
      packages = p: [ drv ];
      buildInputs = runtimeDeps ++ devUtils;
      shellHook = builtins.readFile ./prompt.sh;
    };
  }
