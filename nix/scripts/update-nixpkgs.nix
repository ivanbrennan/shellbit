{ pkgs ? import <nixpkgs> {}
, projectRoot
, owner
, repo
, rev
}:

with pkgs;

let
  url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
  pinned = "${projectRoot}/nix/pinned.json";

in
  mkShell {
    buildInputs = [ jq nix ];

    shellHook = ''
      set -eu

      tmpfile=$(mktemp pinned-XXXX)

      echo "Pre-fetching nixpkgs"
      sha256=$(
        nix-prefetch-url '${url}' --unpack --type sha256 2>/dev/null
      )

      jq ".nixpkgs |= { url: \"${url}\", sha256: \"$sha256\" }" \
          ${pinned} > $tmpfile

      mv $tmpfile ${pinned}
      echo "Updated ${pinned}"

      exit
    '';
  }
