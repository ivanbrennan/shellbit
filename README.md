## shellbit

Launch a nix-shell for the current project.
```sh
shellbit
```

### install from expression

```sh
nix-env -iE '_:
  let
    url = https://github.com/ivanbrennan/shellbit/archive/0.1.0.0.tar.gz;
  in
    (import "${builtins.fetchTarball url}/nix" { }).minimal
'
```

### install / uninstall

```sh
make install
```

```sh
make uninstall
```

### development

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

lint
```sh
make lint
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
