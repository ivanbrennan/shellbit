{-# LANGUAGE OverloadedStrings #-}

module NixShellBit.Sbox
  ( addVersion
  , fixturesPath
  , initialVersion
  , localProject
  , projectName
  , remoteNixShells
  , remoteProject
  , removeVersion
  , setTags
  , setVersions
  , withSandbox
  , xdgConfigPath
  ) where

import Data.Foldable             (traverse_)
import Distribution.Simple.Utils (copyDirectoryRecursive)
import Distribution.Verbosity    (silent)
import NixShellBit.Git           (git_)
import NixShellBit.Config        (configInit)
import System.Directory          (copyFile, createDirectory)
import System.FilePath           ((</>))
import Test.Utils                (silence, withEnv, withInput)

import qualified Data.ByteString.Char8 as BS8
import qualified Test.Sandbox as S


withSandbox :: (S.Sandbox -> IO ()) -> IO ()
withSandbox =
    S.withSandbox initialize
  where
    initialize :: FilePath -> IO ()
    initialize sand =
      let
        xdgConfig    = xdgConfigPath sand
        localProj    = localProject sand
        remoteProj   = remoteProject sand
        remoteShells = remoteNixShells sand
      in
        withEnv
          [ ("XDG_CONFIG_HOME",   Just xdgConfig)
          , ("NIX_SHELL_BIT_URL", Just remoteShells)
          ] $
          do
            createDirectory xdgConfig
            createDirectory (xdgConfig </> "git")
            copyDirectoryRecursive silent "test/fixtures" (fixturesPath sand)

            _ <- withInput ["yes"] (silence configInit)
            BS8.writeFile (xdgConfig </> "git/config") ""

            -- configure sandbox Git user
            git_ ["config", "--global", "user.email", "nobody@example.com"]
            git_ ["config", "--global", "user.name", "nobody"]

            -- setup remote project
            git_ ["init", "--quiet", "--template", "", remoteProj]
            BS8.writeFile (remoteProj </> "VERSION") (BS8.pack initialVersion)
            git_ ["-C", remoteProj, "add", "VERSION"]
            git_ ["-C", remoteProj, "commit", "--quiet", "--message", "x"]

            -- setup local project
            git_ ["clone", "--quiet", "--template", "", remoteProj, localProj]

            -- setup remote derivation
            git_ ["init", "--quiet", "--template", "", remoteShells]
            copyFile "test/fixtures/pkgs.nix" (remoteShells </> "default.nix")
            traverse_ (git_ . (["-C", remoteShells] ++))
              [ ["add", "default.nix"]
              , ["commit", "--quiet", "--message", "x"]
              , ["tag", projectTag initialVersion]
              ]


localProject :: FilePath -> FilePath
localProject sand = sand </> projectName ++ "-local"

remoteProject :: FilePath -> FilePath
remoteProject sand = sand </> projectName

remoteNixShells :: FilePath -> FilePath
remoteNixShells sand = sand </> "NIX_SHELLS"

xdgConfigPath :: FilePath -> FilePath
xdgConfigPath sand = sand </> "XDG_CONFIG_HOME"

fixturesPath :: FilePath -> FilePath
fixturesPath sand = sand </> "fixtures"

projectName :: String
projectName = "PROJECT"

initialVersion :: String
initialVersion = "0.1.0"

projectTag :: String -> String
projectTag v = projectName ++ "-" ++ v


setVersions :: FilePath -> [String] -> IO ()
setVersions sand =
  setTags sand . map projectTag


removeVersion :: FilePath -> String -> IO ()
removeVersion sand =
  removeTag sand . projectTag


addVersion :: FilePath -> String -> IO ()
addVersion sand v =
  git_ ["-C", remoteNixShells sand, "tag", projectTag v]


setTags :: FilePath -> [String] -> IO ()
setTags sand tags =
  removeTag sand (projectTag initialVersion) >>
  traverse_ (addTag sand) tags


removeTag :: FilePath -> String -> IO ()
removeTag sand tag =
  git_ ["-C", remoteNixShells sand, "tag", "--delete", tag]


addTag :: FilePath -> String -> IO ()
addTag sand tag =
  git_ ["-C", remoteNixShells sand, "tag", tag]
