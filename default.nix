{ mkDerivation, aeson, ansi-wl-pprint, attoparsec, base, bytestring
, Cabal, dhall, directory, filemanip, filepath, haskeline, hlibgit2
, hspec, main-tester, optparse-applicative, prettyprinter
, regex-tdfa, safe, stdenv, temporary, terminal-size, text
, transformers, typed-process, unix, unliftio
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
    optparse-applicative prettyprinter safe temporary terminal-size
    text transformers typed-process unix unliftio
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring Cabal directory filemanip filepath hspec
    main-tester optparse-applicative regex-tdfa temporary text
    typed-process unliftio
  ];
  license = stdenv.lib.licenses.bsd3;
}
