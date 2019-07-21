module Shellbit.Operation
  ( Operation(..)
  , OperationError(..)
  , operation
  ) where

import Control.Monad       (unless, when)
import Data.Maybe          (fromMaybe)
import Shellbit.Config  (Config, configInit, cShellbitUrl, cShellbitBranch)
import Shellbit.Git     (gitTaggedVersions)
import Shellbit.Nix     (NixArguments, arguments, derivation)
import Shellbit.Options (Options, Command(Exec, List), optArgs, optCommand,
                            optProject, optVersion)
import Shellbit.Project (Project, currentProject, unProject)
import Shellbit.Version (Version(Version), currentVersion)
import UnliftIO.Exception  (Exception, Typeable, throwIO)


data Operation
  = ListVersions [Version] (Maybe Version)
  | ExecuteShell NixArguments
  deriving (Eq, Show)


data OperationError
  = NoProject
  | NoVersion
  | NoVersionsFound Project
  | VersionNotFound [Version] Version
  deriving (Eq, Show, Typeable)

instance Exception OperationError


operation :: Options -> IO Operation
operation opts =
  do
    config   <- configInit
    project  <- getProject >>= maybe (throwIO NoProject) pure
    versions <- taggedVersions config project
    mversion <- getVersion

    when (null versions)
         (throwIO (NoVersionsFound project))

    case command opts of
      List ->
        pure (ListVersions versions mversion)
      Exec -> do
        version <- maybe (throwIO NoVersion) pure mversion

        unless (version `elem` versions)
               (throwIO (VersionNotFound versions version))

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
      map Version <$> gitTaggedVersions (cShellbitUrl config)
                                        (unProject project)

    command :: Options -> Command
    command = fromMaybe Exec . optCommand

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
        url    = cShellbitUrl config
        branch = cShellbitBranch config
