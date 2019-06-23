module NixShellBit.Main
  ( nixShellBit
  ) where

import Control.Monad       (unless, when)
import Data.Maybe          (fromMaybe)
import Data.Version        (showVersion)
import NixShellBit.Config  (Config, configPath, getConfig, nixShellBitUrl,
                            saveConfig)
import NixShellBit.Git     (gitListVersions)
import NixShellBit.PPrint  (listItems, oopsNoProject, oopsNoVersion,
                            oopsNoVersions)
import NixShellBit.Options (Options, Command(Exec, List), options,
                            optProject, optVersion, optCommand, optArgs)
import NixShellBit.Project (Project, detectProject, unProject)
import NixShellBit.Version (Version, detectVersion, unVersion)
import Options.Applicative (briefDesc, execParser, info, infoOption,
                            helper, hidden, short)
import System.Directory    (doesFileExist)

import qualified Data.Text as T
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

    project <- maybe oopsNoProject pure =<< getProject
    print project

    version <- maybe oopsNoVersion pure =<< getVersion
    print version

    print (optArgs opts)

    versions <- taggedVersions config project

    case command opts of
      List -> do
        when (null versions) (oopsNoVersions $ unProject project)
        listItems (unVersion version) versions
      Exec ->
        print Exec
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

    taggedVersions :: Config -> Project -> IO [String]
    taggedVersions config project =
      let
        url = T.unpack (nixShellBitUrl config)
        pjt = unProject project
      in
        gitListVersions url pjt
