module Shellbit.Nix
  ( NixArguments(..)
  , NixDerivation(..)
  , derivation
  , arguments
  , executeNixShell
  , tmpClonePrefix
  ) where

import Data.Maybe           (fromMaybe)
import Data.Text            (Text)
import Shellbit.Git         (gitArchiveUrl, gitClone)
import Shellbit.Options     (Arg, unArg)
import Shellbit.Project     (Project(Project))
import Shellbit.Version     (Version(Version))
import System.IO.Temp       (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Posix.Process (executeFile)

import qualified Data.Text as T


newtype NixDerivation = NixDerivation String deriving (Eq, Show)

newtype NixArguments = NixArguments [String] deriving (Eq, Show)


derivation
  :: Text
  -> Maybe Text
  -> Project
  -> Version
  -> IO NixDerivation
derivation url branch (Project project) (Version version) =
    NixDerivation <$> maybe tmpClone pure archive
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
        dir <- createTempDirectory tmp (tmpClonePrefix ++ "-" ++ project)
        gitClone url dir ref
        pure dir


arguments
  :: NixDerivation
  -> Project
  -> [Arg]
  -> NixArguments
arguments (NixDerivation drv) (Project project) args =
  NixArguments $ [ drv
                 , "--attr"
                 , project
                 ] ++ map unArg args


executeNixShell :: NixArguments -> IO ()
executeNixShell (NixArguments args) =
    executeFile "nix-shell" search args env
  where
    search :: Bool
    search = True

    env :: Maybe [(String, String)]
    env = Nothing


tmpClonePrefix :: String
tmpClonePrefix = "shellbit-tmp-clone"
