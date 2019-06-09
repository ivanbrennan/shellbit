{ pkgs ? import <nixpkgs> {} }:

let
  drv = (import ./release.nix { inherit pkgs; }).nix-shell-bit;
in
  with pkgs; haskellPackages.shellFor {
    packages = p: [ drv ];
    buildInputs = [ cabal-install hlint ];
  }
