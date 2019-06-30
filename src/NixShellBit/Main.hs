module NixShellBit.Main
  ( nixShellBit
  ) where

import Data.Version          (showVersion)
import NixShellBit.Nix       (executeNixShell)
import NixShellBit.Operation (Operation(ExecuteShell, ListVersions), operation)
import NixShellBit.Options   (options)
import NixShellBit.PPrint    (listItems)
import NixShellBit.Version   (unVersion)
import Options.Applicative   (briefDesc, execParser, info, infoOption, helper,
                              hidden, short)

import qualified Paths_nix_shell_bit as Self


nixShellBit :: IO ()
nixShellBit =
    execParser o >>= operation >>= run
  where
    o = info (helper <*> v <*> options) briefDesc
    v = infoOption (showVersion Self.version) (short 'V' <> hidden)

    run (ListVersions versions mversion) =
      listItems (map unVersion versions)
                (unVersion <$> mversion)

    run (ExecuteShell nixArguments) =
      executeNixShell nixArguments
