module NixShellBit.Main
  ( nixShellBit
  ) where

import Data.Maybe          (fromMaybe)
import Data.Version        (showVersion)
import NixShellBit.Options (Options, Project(Project), Version(Version),
                            Command(Exec), options, optProject,
                            optVersion, optCommand, optArgs)
import Options.Applicative (briefDesc, execParser, info, infoOption,
                            helper, hidden, short)

import qualified Paths_nix_shell_bit as Self


nixShellBit :: IO ()
nixShellBit =
    run =<< execParser o
  where
    o = info (helper <*> v <*> options) briefDesc
    v = infoOption (showVersion Self.version) (short 'V' <> hidden)


run :: Options -> IO ()
run opts = do
    print (project opts)
    print (version opts)
    print (command opts)
    print (optArgs opts)
  where
    project = fromMaybe (Project "currentProject") . optProject
    version = fromMaybe (Version "currentVersion") . optVersion
    command = fromMaybe Exec . optCommand
