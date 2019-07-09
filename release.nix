{ pkgs ? import <nixpkgs> {} }:

with pkgs;
{
  nix-shell-bit =
    haskell.lib.generateOptparseApplicativeCompletion "nix-shell-bit" (
      haskellPackages.callPackage ./default.nix { }
  );
}
