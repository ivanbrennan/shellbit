{-# LANGUAGE OverloadedStrings #-}

module NixShellBit.Sandbox
  ( Sandbox
  , addVersion
  , initialVersion
  , removeVersion
  , sandboxLocalProject
  , sandboxNixShellBitUrl
  , useSandbox
  , withSandbox
  ) where

import Control.Exception         (bracket_)
import Control.Monad             (void)
import Data.Foldable             (traverse_)
import Distribution.Simple.Utils (copyDirectoryRecursive)
import Distribution.Verbosity    (silent)
import NixShellBit.Config        (configInit)
import NixShellBit.Project       (Project(Project), unProject)
import NixShellBit.Version       (Version(Version), unVersion)
import System.Directory          (copyFile, createDirectory,
                                  removeDirectoryRecursive, withCurrentDirectory)
import System.FilePath           ((</>), (<.>))
import System.IO.Temp            (withSystemTempDirectory)
import System.Process.Typed      (proc, readProcess_)
import Test.Main                 (captureProcessResult, withEnv, withStdin)

import qualified Data.ByteString.Char8 as C


newtype Sandbox = Sandbox FilePath


project :: Project
project = Project "PROJECT"

initialVersion :: Version
initialVersion = Version "0.1.0"


tag :: String -> String
tag v = unProject project ++ "-" ++ v


sandboxLocalProject :: Sandbox -> FilePath
sandboxLocalProject =
  (`sbxPath` ("locals" </> unProject project))

sbxRemoteProject :: Sandbox -> FilePath
sbxRemoteProject =
  (`sbxPath` ("remotes" </> unProject project))

sbxXdgConfig :: Sandbox -> FilePath
sbxXdgConfig =
  (`sbxPath` "XDG_CONFIG_HOME")

sandboxNixShellBitUrl :: Sandbox -> FilePath
sandboxNixShellBitUrl =
  (`sbxPath` "NIX_SHELL_BIT_URL")

sbxPath :: Sandbox -> FilePath -> FilePath
sbxPath (Sandbox root) path =
  root </> path

seed :: FilePath -> FilePath
seed = (<.> "seed")


withSandbox
  :: (Sandbox -> IO ())
  -> IO ()
withSandbox action =
    withSystemTempDirectory "nix-shell-bit-sandbox" $ \dir ->
      let
        sbx = Sandbox dir

        localProject  = sandboxLocalProject sbx
        remoteProject = sbxRemoteProject sbx
        shellBitUrl   = sandboxNixShellBitUrl sbx
        xdgConfig     = sbxXdgConfig sbx
      in
        do
          traverse_ createDirectory
            [ sbxPath sbx "locals"
            , sbxPath sbx "remotes"
            , seed xdgConfig
            , seed xdgConfig </> "git"
            ]

          C.writeFile (seed xdgConfig </> "git/config") ""

          withEnv_
            [("XDG_CONFIG_HOME", Just (seed xdgConfig))] $
            do
              -- configure sandbox Git user
              gitConfig "user.email" "nobody@example.com"
              gitConfig "user.name"  "nobody"

              -- setup remote project
              gitInit remoteProject
              C.writeFile (remoteProject </> "VERSION")
                          (C.pack $ unVersion initialVersion)
              gitC remoteProject ["add", "VERSION"]
              gitC remoteProject ["commit", "--quiet", "--message", "version"]

              -- setup local project seed
              gitClone remoteProject (seed localProject)

              -- setup remote derivations
              gitInit (seed shellBitUrl)

              copyFile "test/fixtures/pkgs.nix"
                (seed shellBitUrl </> "default.nix")

              traverse_ (gitC (seed shellBitUrl))
                [ ["add", "default.nix"]
                , ["commit", "--quiet", "--message", "initial commit"]
                , ["tag", tag (unVersion initialVersion)]
                ]

          withEnv_
            [ ("XDG_CONFIG_HOME", Just (seed xdgConfig))
            , ("NIX_SHELL_BIT_URL", Just shellBitUrl)
            ] $
            captureProcessResult (void $ withStdin "yes" configInit)

          withEnv_
            [("XDG_CONFIG_HOME", Just xdgConfig)]
            (action sbx)
  where
    withEnv_
      :: [(String, Maybe String)]
      -> IO a
      -> IO ()
    withEnv_ env = void . withEnv env

    gitConfig :: String -> String -> IO ()
    gitConfig name value =
      git ["config", "--global", name, value]

    gitInit :: FilePath -> IO ()
    gitInit directory =
      git ["init", "--quiet", "--template", "", directory]

    gitClone :: FilePath -> FilePath -> IO ()
    gitClone repository directory =
      git ["clone", "--quiet", "--template", "", repository, directory]


git :: [String] -> IO ()
git = void . readProcess_ . proc "git"


gitC :: FilePath -> [String] -> IO ()
gitC directory args =
  git (["-C", directory] ++ args)


addVersion :: Sandbox -> String -> IO ()
addVersion sbx v =
  gitC (sandboxNixShellBitUrl sbx)
    ["tag", tag v]


removeVersion :: Sandbox -> Version -> IO ()
removeVersion sbx (Version v) =
  gitC (sandboxNixShellBitUrl sbx)
    ["tag", "--delete", tag v]


useSandbox
  :: Sandbox
  -> IO ()
  -> IO ()
useSandbox sbx action =
    bracket_ setup teardown runAction
  where
    setup :: IO ()
    setup =
      traverse_ copyFromSeed testDirectories

    teardown :: IO ()
    teardown =
      traverse_ removeDirectoryRecursive testDirectories

    runAction :: IO ()
    runAction =
      withCurrentDirectory (sandboxLocalProject sbx) action

    copyFromSeed :: FilePath -> IO ()
    copyFromSeed dir =
      copyDirectoryRecursive silent (seed dir) dir

    testDirectories :: [FilePath]
    testDirectories =
      [ sandboxNixShellBitUrl sbx
      , sbxXdgConfig sbx
      , sandboxLocalProject sbx
      ]
