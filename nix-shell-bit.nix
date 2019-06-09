{ mkDerivation, base, cmdargs, hspec, silently, stdenv }:
mkDerivation {
  pname = "nix-shell-bit";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base cmdargs ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec silently ];
  license = stdenv.lib.licenses.bsd3;
}
