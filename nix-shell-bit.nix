{ mkDerivation, aeson, ansi-wl-pprint, attoparsec, base, bytestring
, Cabal, dhall, directory, filemanip, filepath, haskeline, hlibgit2
, hspec, main-tester, optparse-applicative, prettyprinter
, regex-tdfa, safe, stdenv, temporary, text, transformers
, typed-process, unix, unliftio
}:
mkDerivation {
  pname = "nix-shell-bit";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson ansi-wl-pprint attoparsec base bytestring Cabal dhall
    directory filemanip filepath haskeline hlibgit2
    optparse-applicative prettyprinter safe temporary text transformers
    typed-process unix unliftio
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring Cabal directory filepath hspec main-tester
    regex-tdfa temporary text typed-process
  ];
  license = stdenv.lib.licenses.bsd3;
}
