module Shellbit.Main
  ( shellbit
  , toOperation
  ) where

import Data.Version        (showVersion)
import Options.Applicative (briefDesc, execParser, info, infoOption, helper,
                            hidden, short)
import Shellbit.Git        (GitError(GitError))
import Shellbit.Nix        (executeNixShell)
import Shellbit.Operation  (Operation(ExecuteShell, ListVersions),
                            OperationError(NoProject, NoVersion,
                            NoVersionsFound, VersionNotFound), operation)
import Shellbit.Options    (Options, options)
import Shellbit.PPrint     (Doc, fatal, listItems, noProject, noVersion,
                            noVersions, putDocLn, putVersionNotFound)
import Shellbit.Project    (unProject)
import Shellbit.Version    (unVersion)
import System.Exit         (exitFailure)
import UnliftIO.Exception  (Handler(Handler), catches)

import qualified Paths_shellbit as Self


shellbit :: IO ()
shellbit =
    execParser o >>= toOperation >>= run
  where
    o = info (helper <*> v <*> options) briefDesc
    v = infoOption (showVersion Self.version) (short 'V' <> hidden)

    run (ListVersions versions mversion) =
      listItems (map unVersion versions)
                (unVersion <$> mversion)

    run (ExecuteShell nixArguments) =
      executeNixShell nixArguments


toOperation :: Options -> IO Operation
toOperation opts =
    operation opts `catches` [ Handler opError
                             , Handler gitError
                             ]
  where
    opError :: OperationError -> IO a
    opError NoProject              = die noProject
    opError NoVersion              = die noVersion
    opError (NoVersionsFound p)    = die (noVersions (unProject p))
    opError (VersionNotFound vs v) =
      do
        putVersionNotFound (map unVersion vs) (unVersion v)
        exitFailure

    gitError :: GitError -> IO a
    gitError (GitError cmd err) = die (fatal cmd err)

    die :: Doc -> IO a
    die doc =
      do
        putDocLn doc
        exitFailure
