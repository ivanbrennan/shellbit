{-# LANGUAGE OverloadedStrings #-}

module NixShellBit.GitSpec (spec) where

import Control.Monad    (void)
import NixShellBit.Git  (gitArchiveUrl, gitDiscoverRepo, gitRemoteList,
                         gitTaggedVersions)
import NixShellBit.Sbox (setVersions)
import System.Directory (createDirectoryIfMissing)
import System.FilePath  ((</>))
import Test.Hspec       (Spec, before_, context, describe, it, shouldBe,
                         shouldContain, shouldReturn)
import Test.Main        (ExitCode(ExitFailure), captureProcessResult, prExitCode,
                         prStderr)
import Test.Utils       (string)

import qualified Data.Text as T


spec :: FilePath -> Spec
spec sand = do
    describe "gitArchiveUrl" $ do
      context "github" $ do
        let url = "https://github.com/Foo/nix-shells/archive/master.tar.gz"

        it "handles SSH" $
          gitArchiveUrl "git@github.com:Foo/nix-shells.git" "master"
            `shouldBe` Just url

        it "handles HTTPS" $
          gitArchiveUrl "https://github.com/Foo/nix-shells.git" "master"
            `shouldBe` Just url

      context "gitlab" $ do
        let url = "https://gitlab.com/Foo/nix-shells/get/master.tar.gz"

        it "handles SSH" $
          gitArchiveUrl "git@gitlab.com:Foo/nix-shells.git" "master"
            `shouldBe` Just url

        it "handles HTTPS" $
          gitArchiveUrl "https://gitlab.com/Foo/nix-shells.git" "master"
            `shouldBe` Just url

      context "bitbucket" $ do
        let url = "https://bitbucket.org/Foo/nix-shells/repository/archive.tar.gz?ref=master"

        it "handles SSH" $
          gitArchiveUrl "git@bitbucket.org:Foo/nix-shells.git" "master"
            `shouldBe` Just url

        it "handles HTTPS" $
          gitArchiveUrl "https://Foo@bitbucket.org/Foo/nix-shells.git" "master"
            `shouldBe` Just url


    describe "gitDiscoverRepo" $ do
      let startPath = sand </> "local/PROJECT/foo/bar"

      before_ (createDirectoryIfMissing True startPath) $ do
        it "can discover a repo containing startPath" $ do
          let ceilingDirs = [sand]

          gitDiscoverRepo startPath ceilingDirs `shouldReturn`
            Just (sand </> "local/PROJECT/.git/")

        it "won't search past ceilingDirs" $ do
          let ceilingDirs = [sand </> "local/PROJECT/foo"]

          gitDiscoverRepo startPath ceilingDirs `shouldReturn`
            Nothing

    describe "gitRemoteList" $ do
      context "given a repo's toplevel directory" $
        it "lists remotes" $ do
          let path = sand </> "local/PROJECT"

          gitRemoteList path `shouldReturn` ["origin"]

      context "given a repo's .git directory" $
        it "lists remotes" $ do
          let path = sand </> "local/PROJECT/.git"

          gitRemoteList path `shouldReturn` ["origin"]

      context "given any other directory" $
        it "fails" $ do
          let path = sand </> "local/PROJECT/foo"

          createDirectoryIfMissing True path
          r <- captureProcessResult (void $ gitRemoteList path)

          prExitCode r `shouldBe` ExitFailure 1

          string (prStderr r) `shouldContain`
            "Could not find repository"


    describe "gitTaggedVersions" $ do
      let url = T.pack (sand </> "remote/NIX_SHELLS")

      it "lists tags matching prefix" $ do
        setVersions sand []

        gitTaggedVersions url "PROJECT" `shouldReturn` []

      it "is empty if no tags exist" $ do
        setVersions sand []

        gitTaggedVersions url "PROJECT" `shouldReturn` []
