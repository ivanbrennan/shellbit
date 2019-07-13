let
  pinned = builtins.fromJSON (builtins.readFile ./pinned.json);

in import (builtins.fetchTarball pinned.nixpkgs)
