{ projectRoot }:

let
  dev = (import "${projectRoot}/nix" { }).dev;

in
  dev.overrideAttrs (old: rec {
    shellHook = ''
      set -eu

      cd ${projectRoot} >/dev/null

      cabal v2-exec --verbose=0 shellbit \
          -- \
          --bash-completion-script shellbit \
          > completions/_shellbit.bash

      cabal v2-exec --verbose=0 shellbit \
          -- \
          --zsh-completion-script shellbit \
          > completions/_shellbit.zsh

      exit
    '';
  })
