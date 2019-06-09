{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  withCompletions = exeName: pkg: haskell.lib.overrideCabal pkg (drv: {
    postInstall = (drv.postInstall or "") + ''
      bashCompDir="$out/share/bash-completion/completions"
      zshCompDir="$out/share/zsh/vendor-completions"
      fishCompDir="$out/share/fish/vendor_completions.d"
      mkdir -p "$bashCompDir" "$zshCompDir" "$fishCompDir"

      "$out/bin/${exeName}" --bash-completion-script "$out/bin/${exeName}" >"$bashCompDir/${exeName}"
      "$out/bin/${exeName}" --zsh-completion-script "$out/bin/${exeName}" >"$zshCompDir/_${exeName}"
      "$out/bin/${exeName}" --fish-completion-script "$out/bin/${exeName}" >"$fishCompDir/${exeName}.fish"

      sed -i \
          -e 's/^complete\b.* -F /complete -o bashdefault -o default -F /' \
          "$bashCompDir/${exeName}"

      # Sanity check
      grep -F ${exeName} <$bashCompDir/${exeName} >/dev/null || {
        echo 'Could not find ${exeName} in completion script.'
        exit 1
      }
    '';
  });

in {
  nix-shell-bit = withCompletions "nix-shell-bit" (
    haskellPackages.callPackage ./nix-shell-bit.nix { }
  );
}
