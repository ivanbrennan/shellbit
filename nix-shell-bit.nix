{ mkDerivation, base, hlibgit2, hspec, optparse-applicative, stdenv
}:
mkDerivation {
  pname = "nix-shell-bit";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base hlibgit2 optparse-applicative ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  license = stdenv.lib.licenses.bsd3;
}
