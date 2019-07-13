{ pkgs ? import <nixpkgs> {}, projectRoot }:

with pkgs;

mkShell {
  buildInputs = [ cabal2nix ];

  shellHook = ''
    set -eu

    cd ${projectRoot} >/dev/null
    cabal2nix . > default.nix

    exit
  '';
}
