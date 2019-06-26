module NixShellBit.Main
  ( nixShellBit
  ) where

import Control.Monad        (unless, when)
import Data.Maybe           (fromMaybe)
import Data.Version         (showVersion)
import NixShellBit.Config   (Config, configPath, getConfig, nixShellBitUrl,
                             nixShellBitBranch, saveConfig)
import NixShellBit.Git      (URL, Branch, gitListVersions)
import NixShellBit.Nix      (derivation, executeNixShell)
import NixShellBit.Options  (Options, Command(Exec, List), options, optProject,
                             optVersion, optCommand, optArgs)
import NixShellBit.PPrint   (listItems, oopsNoProject, oopsNoVersion,
                             oopsNoVersions, oopsVersionUnavailable)
import NixShellBit.Project  (Project, detectProject, unProject)
import NixShellBit.Version  (Version(Version), detectVersion, unVersion)
import Options.Applicative  (briefDesc, execParser, info, infoOption,
                             helper, hidden, short)
import System.Directory     (doesFileExist)

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
    config   <- loadConfig
    project  <- maybe oopsNoProject pure =<< getProject
    version  <- maybe oopsNoVersion pure =<< getVersion
    versions <- taggedVersions config project

    when (null versions) (oopsNoVersions $ unProject project)

    case command opts of
      List ->
        listItems (unVersion version) versions
      Exec ->
        requireVersion version versions >>
          exec config project version
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
        url = (T.unpack . nixShellBitUrl) config
        pjt = unProject project
      in
        gitListVersions url pjt

    exec :: Config -> Project -> Version -> IO ()
    exec config project version =
      do
        drv <- derivation url branch project version
        executeNixShell drv project (optArgs opts)
      where
        url :: URL
        url = nixShellBitUrl config

        branch :: Maybe Branch
        branch = nixShellBitBranch config

    requireVersion :: Version -> [String] -> IO ()
    requireVersion (Version v) versions
      | v `elem` versions = pure ()
      | otherwise         = oopsVersionUnavailable v versions
