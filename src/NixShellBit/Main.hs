module NixShellBit.Main
  ( nixShellBit
  ) where

import Control.Monad       (when)
import Data.Maybe          (fromMaybe)
import Data.Version        (showVersion)
import NixShellBit.Config  (Config, askConfig, findConfig, readConfig,
                            saveConfig)
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
    config <- getConfig
    print config

    project <- getProject
    maybe oopsNoProject print project

    version <- getVersion
    maybe oopsNoVersion print version

    print (command opts)
    print (optArgs opts)
  where
    getConfig :: IO Config
    getConfig =
      do
        path   <- findConfig
        config <- maybe askConfig readConfig path
        when (null path) (saveConfig config)
        pure config

    getProject :: IO (Maybe Project)
    getProject =
      maybe detectProject (pure . Just) (optProject opts)

    getVersion :: IO (Maybe Version)
    getVersion =
      maybe detectVersion (pure . Just) (optVersion opts)

    command = fromMaybe Exec . optCommand
