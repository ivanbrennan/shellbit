{-# LANGUAGE OverloadedStrings #-}

module NixShellBit.GitSpec (spec) where

import Data.Foldable        (traverse_)
import NixShellBit.Git      (GitError, gitArchiveUrl, gitClone, gitDiscoverRepo,
                             gitRemoteList, gitTaggedVersions, gitRemoteGetUrl)
import NixShellBit.Sbox     (localProject, projectName, remoteProject,
                             remoteNixShells, setTags)
import System.Directory     (createDirectory, createDirectoryIfMissing)
import System.FilePath      ((</>))
import System.Process.Typed (proc, readProcess_, readProcessStdout_)
import Test.Hspec           (Spec, before_, context, describe, it, shouldBe,
                             shouldReturn, shouldThrow)

import qualified Data.ByteString.Lazy.Char8 as BSL8
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
      let startPath = localProject sand </> "foo/bar"

      before_ (createDirectoryIfMissing True startPath) $ do
        it "can discover a repo containing startPath" $ do
          let ceilingDirs = [sand]

          gitDiscoverRepo startPath ceilingDirs `shouldReturn`
            Just (localProject sand </> ".git/")

        it "won't search past ceilingDirs" $ do
          let ceilingDirs = [localProject sand </> "foo"]

          gitDiscoverRepo startPath ceilingDirs `shouldReturn`
            Nothing

    describe "gitRemoteList" $ do
      context "given a repo's root directory" $
        it "lists remotes" $ do
          let path = localProject sand

          gitRemoteList path `shouldReturn` ["origin"]

      context "given a repo's .git directory" $
        it "lists remotes" $ do
          let path = localProject sand </> ".git"

          gitRemoteList path `shouldReturn` ["origin"]

      context "given any other directory" $ do
        let path = localProject sand </> "foo"

        it "throws GitError if directory exists" $ do
          createDirectory path

          gitRemoteList path `shouldThrow` isGitError

        it "throws GitError if directory does not exist" $
          gitRemoteList path `shouldThrow` isGitError


    describe "gitTaggedVersions" $ do
      let url = T.pack (remoteNixShells sand)

      it "is empty if no tags exist" $ do
        setTags sand []

        gitTaggedVersions url projectName `shouldReturn` []

      it "lists tags matching prefix" $ do
        setTags sand [ projectName ++ "-3.3.3"
                     , projectName ++ "-2.2.2"
                     , "foo-5.5.5"
                     , projectName ++ "-1.1.1"
                     ]

        gitTaggedVersions url projectName `shouldReturn`
          ["1.1.1", "2.2.2", "3.3.3"]


    describe "gitClone" $ do
      let url = remoteProject sand
          dest = sand </> "dest"

      it "clones repo to the requested path" $ do
        gitClone (T.pack url) dest "master"

        o <- readProcessStdout_
           $ proc "git" ["-C", dest, "remote", "get-url", "origin"]

        lines (BSL8.unpack o) `shouldBe` lines url

      it "checks out the requested branch" $ do
        traverse_ (readProcess_ . proc "git")
          [ ["-C", url, "checkout", "-b", "foo"]
          , ["-C", url, "commit", "--allow-empty", "--message", "msg"]
          , ["-C", url, "checkout", "master"]
          ]

        gitClone (T.pack url) dest "foo"

        o <- readProcessStdout_ $ proc "git"
             ["-C", dest, "rev-parse", "--abbrev-ref", "HEAD"]

        lines (BSL8.unpack o) `shouldBe` lines "foo"


    describe "gitRemoteGetUrl" $ do
      let repo = localProject sand

      it "returns the remote url" $
        gitRemoteGetUrl repo "origin" `shouldReturn`
          remoteProject sand

      it "throws GitError if repo does not exist" $
        gitRemoteGetUrl (sand </> "garbage") "origin" `shouldThrow`
          isGitError
  where
      isGitError :: GitError -> Bool
      isGitError = const True
