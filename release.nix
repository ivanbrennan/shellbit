{ pkgs ? import <nixpkgs> {} }:

{
  nix-shell-bit = pkgs.haskellPackages.callPackage ./nix-shell-bit.nix { };
}
