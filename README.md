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

## Notes

To exec a command from within a Haskell program, use [`executeFile`](https://hackage.haskell.org/package/unix-2.7.2.2/docs/System-Posix-Process.html#v:executeFile).
This will replace the running Haskell process with the specified command, just like the `exec` shell builtin.

```haskell
import System.Posix.Process

search = True
derivation = "https://github.com/Foo/nix-shells/archive/pkgset.tar.gz"
project = "bar"
nixShellOptions = [] :: [String]
args = [derivation, "--attr", project] ++ nixShellOptions
env = Nothing :: Maybe [(String, String)]

executeFile "nix-shell" search args env
```
