module NixShellBit.Main
  ( nixShellBit
  , toOperation
  ) where

import Data.Version          (showVersion)
import NixShellBit.Git       (GitError(GitError))
import NixShellBit.Nix       (executeNixShell)
import NixShellBit.Operation (Operation(ExecuteShell, ListVersions),
                              OperationError(NoProject, NoVersion,
                              NoVersionsFound, VersionNotFound), operation)
import NixShellBit.Options   (Options, options)
import NixShellBit.PPrint    (Doc, fatal, listItems, noProject, noVersion,
                              noVersions, putDocLn, putVersionNotFound)
import NixShellBit.Project   (unProject)
import NixShellBit.Version   (unVersion)
import Options.Applicative   (briefDesc, execParser, info, infoOption, helper,
                              hidden, short)
import System.Exit           (exitFailure)
import UnliftIO.Exception    (Handler(Handler), catches)

import qualified Paths_nix_shell_bit as Self


nixShellBit :: IO ()
nixShellBit =
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
