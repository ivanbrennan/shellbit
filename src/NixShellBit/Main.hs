module NixShellBit.Main
  ( nixShellBit
  ) where

import NixShellBit.CmdLine (getCmd)


nixShellBit :: IO ()
nixShellBit =
  getCmd >>= print
