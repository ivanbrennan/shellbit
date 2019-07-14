## nix-shell-bit

Launch a nix-shell for the current project.
```sh
nix-shell-bit
```

## install / uninstall

```sh
make install
```

```sh
make uninstall
```

## development

build
```sh
make
```

run tests
```sh
make test
```

integration tests
```sh
make integration-test
```

build nix package
```sh
make nix-build
```

generate completion scripts
```sh
make completions
```

update nixpkgs revision
```sh
make update-nixpkgs REV=bc94dcf500286495e3c478a9f9322debc94c4304
```

## Todos

- [ ] replace column with a Haskell implementation
- [ ] use conduit-extra
