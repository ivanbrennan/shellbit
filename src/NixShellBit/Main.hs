module NixShellBit.Main
  ( Operation(..)
  , nixShellBit
  , operation
  ) where

import Control.Monad       (unless, when)
import Data.Maybe          (fromMaybe)
import Data.Version        (showVersion)
import NixShellBit.Config  (Config, configInit, nixShellBitUrl, nixShellBitBranch)
import NixShellBit.Git     (gitTaggedVersions)
import NixShellBit.Nix     (NixArguments, arguments, derivation, executeNixShell)
import NixShellBit.Options (Options, Command(Exec, List), options, optArgs,
                            optCommand, optProject, optVersion)
import NixShellBit.PPrint  (listItems, oopsNoProject, oopsNoVersion,
                            oopsNoVersions, oopsVersionUnavailable)
import NixShellBit.Project (Project, detectProject, unProject)
import NixShellBit.Version (Version(Version), detectVersion, unVersion)
import Options.Applicative (briefDesc, execParser, info, infoOption, helper,
                            hidden, short)

import qualified Paths_nix_shell_bit as Self


data Operation
  = ListVersions [Version] (Maybe Version)
  | ExecuteShell NixArguments


nixShellBit :: IO ()
nixShellBit =
    execParser o >>= operation >>= run
  where
    o = info (helper <*> v <*> options) briefDesc
    v = infoOption (showVersion Self.version) (short 'V' <> hidden)

    run (ListVersions versions mversion) =
      listItems (map unVersion versions)
                (unVersion <$> mversion)

    run (ExecuteShell nixArguments) =
      executeNixShell nixArguments


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
      maybe detectProject (pure . Just) (optProject opts)

    getVersion :: IO (Maybe Version)
    getVersion =
      maybe detectVersion (pure . Just) (optVersion opts)

    taggedVersions :: Config -> Project -> IO [Version]
    taggedVersions config project =
      map Version <$> gitTaggedVersions (nixShellBitUrl config)
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
        url    = nixShellBitUrl config
        branch = nixShellBitBranch config
