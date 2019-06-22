module NixShellBit.Main
  ( nixShellBit
  ) where

import Control.Monad       (unless)
import Data.Maybe          (fromMaybe)
import Data.Version        (showVersion)
import NixShellBit.Config  (Config, configPath, getConfig, saveConfig)
import NixShellBit.PPrint  (oopsNoProject, oopsNoVersion)
import NixShellBit.Options (Options, Command(Exec), options, optProject,
                            optVersion, optCommand, optArgs)
import NixShellBit.Project (Project, detectProject)
import NixShellBit.Version (Version, detectVersion)
import Options.Applicative (briefDesc, execParser, info, infoOption,
                            helper, hidden, short)
import System.Directory    (doesFileExist)

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
    config <- loadConfig
    print config

    project <- getProject
    maybe oopsNoProject print project

    version <- getVersion
    maybe oopsNoVersion print version

    print (command opts)
    print (optArgs opts)
  where
    loadConfig :: IO Config
    loadConfig =
      do
        config <- getConfig
        exists <- doesFileExist =<< configPath
        unless exists (saveConfig config)
        pure config

    getProject :: IO (Maybe Project)
    getProject =
      maybe detectProject (pure . Just) (optProject opts)

    getVersion :: IO (Maybe Version)
    getVersion =
      maybe detectVersion (pure . Just) (optVersion opts)

    command = fromMaybe Exec . optCommand
