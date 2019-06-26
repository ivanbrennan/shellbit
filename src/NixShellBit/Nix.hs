module NixShellBit.Nix
  ( derivation
  , executeNixShell
  ) where

import Data.Maybe           (fromMaybe)
import Data.Text            (Text)
import NixShellBit.Git      (gitArchiveUrl, gitClone)
import NixShellBit.Options  (Arg, unArg)
import NixShellBit.Project  (Project(Project))
import NixShellBit.Version  (Version(Version))
import System.IO.Temp       (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Posix.Process (executeFile)

import qualified Data.Text as T


derivation
  :: Text
  -> Maybe Text
  -> Project
  -> Version
  -> IO String
derivation url branch (Project project) (Version version) =
    maybe tmpClone pure archive
  where
    ref :: Text
    ref = fromMaybe tag branch

    tag :: Text
    tag = T.pack (project ++ "-" ++ version)

    archive :: Maybe String
    archive = T.unpack <$> gitArchiveUrl url ref

    tmpClone :: IO String
    tmpClone =
      do
        tmp <- getCanonicalTemporaryDirectory
        dir <- createTempDirectory tmp ("nix-shell-bit-" ++ project)
        gitClone url dir ref
        pure dir


executeNixShell :: String -> Project -> [Arg] -> IO ()
executeNixShell drv (Project project) args =
    executeFile "nix-shell" search arguments env
  where
    search    = True
    env       = Nothing
    arguments = [ drv
                , "--attr"
                , project
                ] ++ map unArg args
