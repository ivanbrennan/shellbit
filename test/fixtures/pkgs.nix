{
  PROJECT = (import <nixpkgs> {}).mkShell {
    name = "PROJECT";
    version = "0.1.0";
    shellHook = ''
      echo "Entered $name $version environment"
      [ "''${AUTO-exit}" == 'exit' ] && exit
    '';
  };
}
