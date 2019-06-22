module NixShellBit.Main
  ( nixShellBit
  ) where

import Data.Maybe          (fromMaybe)
import Data.Version        (showVersion)
import NixShellBit.Config  (Config, askConfig, readConfig)
import NixShellBit.PPrint  (oopsNoProject, oopsNoVersion)
import NixShellBit.Options (Options, Command(Exec), options, optProject,
                            optVersion, optCommand, optArgs)
import NixShellBit.Project (Project, detectProject)
import NixShellBit.Version (Version, detectVersion)
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
run opts =
  do
    config >>= print
    project >>= maybe oopsNoProject print
    version >>= maybe oopsNoVersion print
    print (command opts)
    print (optArgs opts)
  where
    config :: IO Config
    config =
      maybe askConfig pure =<< readConfig

    project :: IO (Maybe Project)
    project =
      maybe detectProject (pure . Just) (optProject opts)

    version :: IO (Maybe Version)
    version =
      maybe detectVersion (pure . Just) (optVersion opts)

    command = fromMaybe Exec . optCommand
