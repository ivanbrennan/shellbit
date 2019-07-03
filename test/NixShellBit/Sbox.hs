{-# LANGUAGE OverloadedStrings #-}

module NixShellBit.Sbox
  ( withSandbox
  , addVersion
  , removeVersion
  , setVersions
  ) where

import Data.Foldable      (traverse_)
import NixShellBit.Git    (git_)
import NixShellBit.Config (configInit)
import System.Directory   (copyFile, createDirectory)
import System.FilePath    ((</>))
import Test.Main          (withEnv, withStdin)
import Test.Utils         (silence)

import qualified Data.ByteString.Char8 as C
import qualified Test.Sandbox as S


withSandbox :: (S.Sandbox -> IO ()) -> IO ()
withSandbox =
    S.withSandbox initialize
  where
    initialize :: FilePath -> IO ()
    initialize sand =
      let
        local     = sand </> "local"
        remote    = sand </> "remote"
        xdgConfig = sand </> "XDG_CONFIG_HOME"

        localProject  = local  </> "PROJECT"
        remoteProject = remote </> "PROJECT"
        remoteShells  = remote </> "NIX_SHELLS"
      in
        withEnv
          [ ("XDG_CONFIG_HOME", Just xdgConfig)
          , ("NIX_SHELL_BIT_URL", Just remoteShells)
          ] $
          do
            traverse_ createDirectory
              [ local
              , remote
              , xdgConfig
              , xdgConfig </> "git"
              ]

            _ <- withStdin "yes" (silence configInit)
            C.writeFile (xdgConfig </> "git/config") ""

            -- configure sandbox Git user
            git_ ["config", "--global", "user.email", "nobody@example.com"]
            git_ ["config", "--global", "user.name", "nobody"]

            -- setup remote project
            git_ ["init", "--quiet", "--template", "", remoteProject]
            C.writeFile (remoteProject </> "VERSION") "0.1.0" -- initialVersion
            git_ ["-C", remoteProject, "add", "VERSION"]
            git_ ["-C", remoteProject, "commit", "--quiet", "--message", "x"]

            -- setup local project
            git_ ["clone", "--quiet", "--template", "", remoteProject, localProject]

            -- setup remote derivation
            git_ ["init", "--quiet", "--template", "", remoteShells]
            copyFile "test/fixtures/pkgs.nix" (remoteShells </> "default.nix")
            traverse_ (git_ . (["-C", remoteShells] ++))
              [ ["add", "default.nix"]
              , ["commit", "--quiet", "--message", "x"]
              , ["tag", "PROJECT-0.1.0"] -- initialVersion
              ]


setVersions :: FilePath -> [String] -> IO ()
setVersions sand vs =
  removeVersion sand "0.1.0" >> -- initialVersion
  traverse_ (addVersion sand) vs


removeVersion :: FilePath -> String -> IO ()
removeVersion sand v =
  git_ ["-C", sand </> "remote/NIX_SHELLS", "tag", "--delete", "PROJECT-" ++ v]


addVersion :: FilePath -> String -> IO ()
addVersion sand v =
  git_ ["-C", sand </> "remote/NIX_SHELLS", "tag", "PROJECT-" ++ v] -- PROJECT (initialVersion similar)
