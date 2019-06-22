module NixShellBit.Project
  ( Project(..)
  , detectProject
  ) where

import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Data.List        (sortOn)
import NixShellBit.Git  (gitDiscoverRepo, gitRemoteList, gitRemoteGetUrl)
import Safe             (headMay)
import System.Directory (getCurrentDirectory, getHomeDirectory)
import System.FilePath  (takeBaseName)


newtype Project
  = Project String
  deriving Show


detectProject :: IO (Maybe Project)
detectProject =
  do
    startPath   <- getCurrentDirectory
    ceilingDirs <- pure <$> getHomeDirectory
    runMaybeT $
      do
        repo   <- detectRepo startPath ceilingDirs
        remote <- detectRemote repo

        MaybeT
         . fmap (Just . Project . takeBaseName)
         $ gitRemoteGetUrl repo remote
  where
    detectRepo :: FilePath -> [String] -> MaybeT IO FilePath
    detectRepo startPath ceilingDirs =
      MaybeT (gitDiscoverRepo startPath ceilingDirs)

    detectRemote :: FilePath -> MaybeT IO String
    detectRemote repo =
      MaybeT (prefer "origin" <$> gitRemoteList repo)

    prefer :: String -> [String] -> Maybe String
    prefer fav =
      headMay . sortOn (/= fav)
