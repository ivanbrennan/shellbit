{ mkDerivation, aeson, ansi-wl-pprint, base, bytestring, Cabal
, dhall, directory, filemanip, filepath, hlibgit2, hspec
, optparse-applicative, prettyprinter, safe, stdenv, text
, transformers, typed-process
}:
mkDerivation {
  pname = "nix-shell-bit";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson ansi-wl-pprint base bytestring Cabal dhall directory
    filemanip filepath hlibgit2 optparse-applicative prettyprinter safe
    text transformers typed-process
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  license = stdenv.lib.licenses.bsd3;
}
