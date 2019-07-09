## nix-shell-bit

Launch a nix-shell for the current project.

```sh
nix-shell-bit
```

simple tests
```sh
cabal v2-test
```

enriched tests
```sh
cabal v2-run nix-shell-bit-test \
    --ghc-options=-Werror \
    -- \
    --failure-report=$PWD/.hspec-failures \
    --rerun-all-on-success \
    --rerun
```

build
```sh
cabal v2-build --ghc-options=-Werror
```

## notes

update default.nix
```sh
cabal2nix . > default.nix
rm -f .ghc.environment.*
```

generate bash/zsh completion scripts:
```sh
cabal v2-build --ghc-options=-Werror &&
cabal v2-exec --verbose=0 nix-shell-bit -- \
    --bash-completion-script nix-shell-bit \
    > completions/_nix-shell-bit.bash &&
cabal v2-exec --verbose=0 nix-shell-bit -- \
    --zsh-completion-script nix-shell-bit \
    > completions/_nix-shell-bit.zsh
```

install nix package
```sh
rm -f .ghc.environment.*
nix-env --install --file release.nix --attr nix-shell-bit
```

Todos:
- [ ] use conduit-extra
- [ ] pin nixpkgs and ghc
- [x] nix derivation
