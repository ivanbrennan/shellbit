{ bash
, coreutils
, findutils
, gawk
, git
, gnugrep
, gnused
, lib
, makeWrapper
, ncurses
, nix
, perl
, runCommand
, stdenv
, utillinux
}:

let
  propagatedBuildInputs = [
    bash
    coreutils
    findutils
    gawk
    git
    gnugrep
    gnused
    ncurses
    nix
    perl
    utillinux
  ];

in runCommand "nix-shell-bit" {
  inherit propagatedBuildInputs;

  nativeBuildInputs = [ makeWrapper ];

  meta = {
    description = "nix-shell tool to find/use remote derivations";
    license = lib.licenses.mit;
    platforms = lib.platforms.linux ++ lib.platforms.darwin;
  };
} ''
  install -D -m755 \
      ${./bin}/$name \
      $out/bin/$name

  install -D -m755 \
      ${./libexec}/archive-url \
      $out/libexec/archive-url

  install -D -m644 \
      ${./completions}/_$name.bash \
      $out/share/bash-completion/completions/$name

  sed -i "s|^LIBEXEC=.*|LIBEXEC=$out/libexec|" $out/bin/$name

  patchShebangs --host $out/bin
  patchShebangs --host $out/libexec

  ${stdenv.shell} -n $out/bin/$name

  wrapProgram $out/bin/$name \
      --prefix PATH : "${lib.makeBinPath propagatedBuildInputs}"

  mkdir -p $out/nix-support
  printWords ${toString propagatedBuildInputs} \
      > $out/nix-support/propagated-build-inputs
''
