## nix-shell-bit

Launch a nix-shell for the current project.

```sh
nix-shell-bit
```

## run tests

simple
```sh
test/main
```

pure
```sh
nix-shell --keep NIX_PATH --pure -p nix --run test/main
```
