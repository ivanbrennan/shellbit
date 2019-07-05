module NixShellBit.MainSpec (spec) where

import Data.List        (intercalate)
import NixShellBit.Main (nixShellBit)
import NixShellBit.Sbox (initialVersion, localProject, remoteNixShells,
                         removeVersion, setVersions, xdgConfigPath)
import System.Directory (removeDirectoryRecursive, removeFile)
import System.FilePath  ((</>))
import Test.Hspec       (Spec, before_, context,
                         describe, it, shouldBe, shouldContain, shouldNotContain)
import Test.Main        (ExitCode(ExitFailure, ExitSuccess),
                         captureProcessResult, prExitCode, prStderr, prStdout,
                         withArgs, withEnv)
import Test.Utils       (captureStderr, shouldMatch, string, withInput)


spec :: FilePath -> Spec
spec sand =
    describe "nix-shell-bit" $ do
      it "prompts for NIX_SHELL_BIT_URL if not configured" $ do
        removeFile (xdgConfigPath sand </> "nix-shell-bit/config.dhall")

        e <- withArgs ["--list"]
           . withInput [remoteNixShells sand, "yes"]
           $ captureStderr nixShellBit

        string e `shouldContain` "Please enter NIX_SHELL_BIT_URL"


      context "when PROJECT cannot be detected" $
        before_ (removeDirectoryRecursive (localProject sand </> ".git")) $ do

        it "shows error" $ do
          result <- captureProcessResult nixShellBit

          string (prStderr result) `shouldContain` "detect project"


        it "returns non-zero exit code" $ do
          result <- captureProcessResult nixShellBit

          prExitCode result `shouldBe` ExitFailure 1


      context "when VERSION cannot be detected" $
        before_ (removeFile (localProject sand </> "VERSION")) $ do

        it "shows error" $ do
          result <- captureProcessResult nixShellBit

          string (prStderr result) `shouldContain` "detect version"


        it "returns non-zero exit code" $ do
          result <- captureProcessResult nixShellBit

          prExitCode result `shouldBe` ExitFailure 1


        it "returns zero exit code if --list flag present" $ do
          result <- withArgs ["--list"]
                    (captureProcessResult nixShellBit)

          prExitCode result `shouldBe` ExitSuccess


      it "returns non-zero exit if NIX_SHELL_BIT_URL cannot be read" $ do
        result <- withEnv
                  [("NIX_SHELL_BIT_URL", Just "/dev/null")]
                  (captureProcessResult nixShellBit)

        prExitCode result `shouldBe` ExitFailure 1


      it "returns non-zero exit if NIX_SHELL_BIT_BRANCH cannot be read" $ do
        result <- withEnv
                  [("NIX_SHELL_BIT_BRANCH", Just "fnord")]
                  (captureProcessResult nixShellBit)

        prExitCode result `shouldBe` ExitFailure 1


      context "when no versions are available for PROJECT" $
        before_ (setVersions sand []) $ do

        it "shows error" $ do
          result <- captureProcessResult nixShellBit

          string (prStderr result) `shouldContain` "No versions"


        it "returns non-zero exit code" $ do
          result <- captureProcessResult nixShellBit

          prExitCode result `shouldBe` ExitFailure 1


      context "when VERSION is unavailable" $
        before_ (setVersions sand ["0.0.5", "0.0.6"]) $ do

        it "shows error" $ do
          result <- captureProcessResult nixShellBit

          string (prStderr result) `shouldContain`
            (initialVersion ++ " not found")


        it "returns non-zero exit code" $ do
          result <- captureProcessResult nixShellBit

          prExitCode result `shouldBe` ExitFailure 1


        it "lists available versions" $ do
          result <- captureProcessResult nixShellBit

          string (prStderr result) `shouldMatch`
            intercalate ".*" [ "\\<0\\.0\\.5\\>"
                             , "\\<0\\.0\\.6\\>"
                             ]


      context "when --list flag is present" $
        before_ (setVersions sand ["0.0.9", "0.1.0", "0.1.1"]) $ do

        it "lists available versions" $ do
          result <- withArgs ["--list"]
                    (captureProcessResult nixShellBit)

          string (prStdout result) `shouldMatch`
            intercalate ".*" [ "\\<0\\.0\\.9\\>"
                             , "\\<0\\.1\\.0\\>"
                             , "\\<0\\.1\\.1\\>"
                             ]


        it "returns zero exit code" $ do
          result <- withArgs ["--list"]
                    (captureProcessResult nixShellBit)

          prExitCode result `shouldBe` ExitSuccess


        it "does not warn about unavailable VERSION" $ do
          removeVersion sand initialVersion
          result <- withArgs ["--list"]
                    (captureProcessResult nixShellBit)

          string (prStdout result) `shouldNotContain`
            (initialVersion ++ " not found")
