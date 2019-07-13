{ projectRoot }:

let
  dev = (import "${projectRoot}/nix" { }).dev;

in
  dev.overrideAttrs (old: rec {
    shellHook = ''
      set -eu

      cd ${projectRoot} >/dev/null

      cabal v2-exec --verbose=0 nix-shell-bit \
          -- \
          --bash-completion-script nix-shell-bit \
          > completions/_nix-shell-bit.bash

      cabal v2-exec --verbose=0 nix-shell-bit \
          -- \
          --zsh-completion-script nix-shell-bit \
          > completions/_nix-shell-bit.zsh

      exit
    '';
  })
