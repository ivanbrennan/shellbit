module NixShellBit.Main
  ( nixShellBit
  ) where

import NixShellBit.Options (cmdline)
import Options.Applicative (briefDesc, execParser, info, helper)


nixShellBit :: IO ()
nixShellBit =
    print =<< execParser opts
  where
    opts = info (helper <*> cmdline) briefDesc
