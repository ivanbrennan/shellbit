## shellbit

Launch a nix-shell for the current project.
```sh
shellbit
```

The first time you run `shellbit`, it will prompt you for the url of the git
repo where your nix-shell recipes are kept.

An example repo can be found at `git@github.com:ivanbrennan/nixels.git`
([nixels](https://github.com/ivanbrennan/nixels)).

If you choose to save this config, subsequent uses will work without prompting.

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

### similar projects

* Repository to maintain out-of-tree shell.nix files: https://github.com/nix-community/nix-environments
