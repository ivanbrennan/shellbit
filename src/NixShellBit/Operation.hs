module NixShellBit.Operation
  ( Operation(..)
  , operation
  ) where

import Control.Monad       (unless, when)
import Data.Maybe          (fromMaybe)
import NixShellBit.Config  (Config, configInit, cNixShellBitUrl, cNixShellBitBranch)
import NixShellBit.Git     (gitTaggedVersions)
import NixShellBit.Nix     (NixArguments, arguments, derivation)
import NixShellBit.Options (Options, Command(Exec, List), optArgs, optCommand,
                            optProject, optVersion)
import NixShellBit.PPrint  (oopsNoProject, oopsNoVersion, oopsNoVersions,
                            oopsVersionUnavailable)
import NixShellBit.Project (Project, currentProject, unProject)
import NixShellBit.Version (Version(Version), currentVersion, unVersion)


data Operation
  = ListVersions [Version] (Maybe Version)
  | ExecuteShell NixArguments


operation :: Options -> IO Operation
operation opts =
  do
    config   <- configInit
    project  <- getProject >>= maybe oopsNoProject pure
    versions <- taggedVersions config project
    mversion <- getVersion

    when (null versions)
         (oopsNoVersions (unProject project))

    case command opts of
      List ->
        pure (ListVersions versions mversion)
      Exec -> do
        version <- maybe oopsNoVersion pure mversion

        unless (version `elem` versions)
               (oopsUnavailable versions version)

        ExecuteShell <$> nixArguments config project version
  where
    getProject :: IO (Maybe Project)
    getProject =
      maybe currentProject (pure . Just) (optProject opts)

    getVersion :: IO (Maybe Version)
    getVersion =
      maybe currentVersion (pure . Just) (optVersion opts)

    taggedVersions :: Config -> Project -> IO [Version]
    taggedVersions config project =
      map Version <$> gitTaggedVersions (cNixShellBitUrl config)
                                        (unProject project)

    command :: Options -> Command
    command = fromMaybe Exec . optCommand

    oopsUnavailable :: [Version] -> Version -> IO ()
    oopsUnavailable versions version =
      oopsVersionUnavailable (map unVersion versions)
                             (unVersion version)

    nixArguments
      :: Config
      -> Project
      -> Version
      -> IO NixArguments
    nixArguments config project version =
      do
        drv <- derivation url branch project version
        pure $ arguments drv project (optArgs opts)
      where
        url    = cNixShellBitUrl config
        branch = cNixShellBitBranch config
