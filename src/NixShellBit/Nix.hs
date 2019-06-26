module NixShellBit.Nix
  ( derivation
  , executeNixShell
  ) where

import Data.Maybe           (fromMaybe)
import Data.Text            (Text, pack, unpack)
import NixShellBit.Git      (Branch, URL, gitArchiveUrl, gitClone)
import NixShellBit.Options  (Arg, unArg)
import NixShellBit.Project  (Project, unProject)
import NixShellBit.Version  (Version, unVersion)
import System.IO.Temp       (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Posix.Process (executeFile)


derivation
  :: URL
  -> Maybe Branch
  -> Project
  -> Version
  -> IO String
derivation url branch project version =
    maybe tmpClone (pure . unpack) archive
  where
    ref :: Text
    ref = fromMaybe tag branch

    tag :: Text
    tag = pack
        $ unProject project ++ "-" ++ unVersion version

    archive :: Maybe Text
    archive = gitArchiveUrl url ref

    tmpClone :: IO String
    tmpClone =
      do
        tmp <- getCanonicalTemporaryDirectory
        dir <- createTempDirectory tmp name
        gitClone (unpack url) dir (unpack ref)
        pure dir
      where
        name = "nix-shell-bit-" ++ unProject project


executeNixShell :: String -> Project -> [Arg] -> IO ()
executeNixShell drv project args =
  let
    search = True
    env    = Nothing

    args' = [ drv
            , "--attr"
            , unProject project
            ] ++ map unArg args
  in
    executeFile "nix-shell" search args' env
