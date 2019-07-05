module NixShellBit.Project
  ( Project(..)
  , currentProject
  , detectProject
  ) where

import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Data.List                 (sortOn)
import NixShellBit.Git           (gitDiscoverRepo, gitRemoteGetUrl, gitRemoteList)
import Safe                      (headMay)
import System.Directory          (getCurrentDirectory, getHomeDirectory)
import System.FilePath           (takeBaseName)


newtype Project = Project
  { unProject :: String
  } deriving (Eq, Show)


currentProject :: IO (Maybe Project)
currentProject =
  do
    startPath   <- getCurrentDirectory
    ceilingDirs <- pure <$> getHomeDirectory
    repo        <- gitDiscoverRepo startPath ceilingDirs

    maybe (pure Nothing) detectProject repo


detectProject :: FilePath -> IO (Maybe Project)
detectProject repo =
    runMaybeT (detectRemote >>= getUrl >>= baseName)
  where
    detectRemote :: MaybeT IO String
    detectRemote =
      MaybeT (prefer "origin" <$> gitRemoteList repo)

    prefer :: String -> [String] -> Maybe String
    prefer fav =
      headMay . sortOn (/= fav)

    getUrl :: String -> MaybeT IO String
    getUrl remote =
      MaybeT (Just <$> gitRemoteGetUrl repo remote)

    baseName :: String -> MaybeT IO Project
    baseName =
      MaybeT . pure . Just . Project . takeBaseName
