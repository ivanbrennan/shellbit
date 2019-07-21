module Shellbit.Project
  ( Project(..)
  , currentProject
  , detectProject
  ) where

import Data.List        (sortOn)
import Shellbit.Git  (gitDiscoverRepo, gitRemoteGetUrl, gitRemoteList)
import Safe             (headMay)
import System.Directory (getCurrentDirectory, getHomeDirectory)
import System.FilePath  (takeBaseName)


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
  do
    remotes <- gitRemoteList repo

    case prefer "origin" remotes of
      Nothing -> pure Nothing
      Just o  -> Just . basename <$> gitRemoteGetUrl repo o
  where
    prefer :: String -> [String] -> Maybe String
    prefer fav =
      headMay . sortOn (/= fav)

    basename :: String -> Project
    basename = Project . takeBaseName
