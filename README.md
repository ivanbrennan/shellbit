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

To build and try in the nix shell:

```sh
nix-shell
cabal v2-build --ghc-options=-Werror
cabal v2-exec nix-shell-bit -- --help
```

To generate bash/zsh completion scripts:

```sh
nix-shell --run '
  cabal v2-build --ghc-options=-Werror &&
  cabal v2-exec nix-shell-bit -- \
      --bash-completion-script nix-shell-bit \
      | sed 's/^complete\b.* -F /complete -o bashdefault -o default -F /' \
      > completions/_nix-shell-bit.bash &&
  cabal v2-exec nix-shell-bit -- \
      --zsh-completion-script nix-shell-bit \
      > completions/_nix-shell-bit.zsh
'
```

To install haskell implementation:
```sh
rm -f .ghc.environment.*
nix-env --install --file release.nix --attr nix-shell-bit
```

```
nix-shell --run 'cabal v2-build --ghc-options=-Werror'
: ${oldPath:=$PATH}
PATH=$PWD/dist-newstyle/build/x86_64-linux/ghc-8.6.5/nix-shell-bit-0.1.0.0/x/nix-shell-bit/build/nix-shell-bit:$PATH
. <(nix-shell-bit --bash-completion-script nix-shell-bit \
    | sed 's/^complete\b.* -F /complete -o bashdefault -o default -F /')
```

Todos:
- [ ] mkCompleter (project, trailing args, etc.)
- [ ] https://haskell.fpcomplete.com/library/optparse-applicative
- [ ] use conduit-extra
- [ ] pin nixpkgs and ghc
