module NixShellBit.Main
  ( nixShellBit
  ) where

import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Data.Maybe          (fromMaybe)
import Data.List           (sortOn)
import Data.Version        (showVersion)
import NixShellBit.Git     (discoverRepo, gitRemoteList, gitRemoteGetUrl)
import NixShellBit.PPrint  (text, vcat, yellow, line, die)
import NixShellBit.Options (Options, Project(Project), Version(Version),
                            Command(Exec), options, optProject,
                            optVersion, optCommand, optArgs)
import Options.Applicative (briefDesc, execParser, info, infoOption,
                            helper, hidden, short)
import Safe                (headMay)
import System.Directory    (getCurrentDirectory, getHomeDirectory)
import System.FilePath     (takeBaseName)

import qualified Paths_nix_shell_bit as Self


nixShellBit :: IO ()
nixShellBit =
    run =<< execParser o
  where
    o = info (helper <*> v <*> options) briefDesc
    v = infoOption (showVersion Self.version) (short 'V' <> hidden)


run :: Options -> IO ()
run opts = do
    project >>= maybe oopsNoProject print
    print (version opts)
    print (command opts)
    print (optArgs opts)
  where
    project :: IO (Maybe Project)
    project =
      case optProject opts of
        Just pjt -> pure (Just pjt)
        Nothing  -> do
          startPath   <- getCurrentDirectory
          ceilingDirs <- pure <$> getHomeDirectory
          runMaybeT (detectProject startPath ceilingDirs)

    version = fromMaybe (Version "currentVersion") . optVersion
    command = fromMaybe Exec . optCommand


detectProject
  :: FilePath
  -> [String]
  -> MaybeT IO Project
detectProject startPath ceilingDirs = do
    repoPath <- MaybeT (discoverRepo startPath ceilingDirs)
    remote   <- MaybeT (gitRemoteList repoPath >>= prefer "origin")

    MaybeT
     . fmap (Just . Project . takeBaseName)
     $ gitRemoteGetUrl repoPath remote
  where
    prefer :: String -> [String] -> IO (Maybe String)
    prefer favorite xs =
      pure . headMay $ sortOn (/= favorite) xs


oopsNoProject :: IO a
oopsNoProject =
  die $ vcat
      [ yellow (text "Could not detect project")
      , text "Try --project=PROJECT"
      , text "(or --help for more options)"
      , line
      ]
