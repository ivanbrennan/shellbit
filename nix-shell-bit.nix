{ mkDerivation, aeson, ansi-wl-pprint, base, bytestring, Cabal
, directory, filemanip, filepath, hlibgit2, hspec
, optparse-applicative, safe, stdenv, transformers
}:
mkDerivation {
  pname = "nix-shell-bit";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson ansi-wl-pprint base bytestring Cabal directory filemanip
    filepath hlibgit2 optparse-applicative safe transformers
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  license = stdenv.lib.licenses.bsd3;
}
