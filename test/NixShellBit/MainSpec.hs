{-# LANGUAGE OverloadedStrings #-}

module NixShellBit.MainSpec (spec) where

import Data.ByteString     (ByteString)
import Data.Foldable       (traverse_)
import NixShellBit.Config  (configPath)
import NixShellBit.Main    (nixShellBit)
import NixShellBit.Sandbox (Sandbox, addVersion, initialVersion, removeVersion,
                            sandboxLocalProject, sandboxNixShellBitUrl)
import NixShellBit.Version (unVersion)
import System.Directory    (doesFileExist, removeDirectoryRecursive, removeFile)
import System.FilePath     ((</>))
import Test.Hspec          (Expectation, HasCallStack, Spec, before_, context,
                            describe, it, shouldBe)
import Test.Main           (ExitCode(ExitFailure, ExitSuccess),
                            captureProcessResult, prExitCode, prStderr, prStdout,
                            withArgs, withEnv, withStdin)
import Text.Regex.TDFA     ((=~))

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C


spec :: Sandbox -> Spec
spec sbx =
    describe "nix-shell-bit" $ do
      it "shows Usage if --help option is used" $ do
        result <- withArgs ["--help"]
                  (captureProcessResult nixShellBit)

        prStdout result `shouldContain` "Usage"


      context "when PROJECT cannot be detected" $
        before_ removeGitDir $ do

        it "shows error" $ do
          result <- captureProcessResult nixShellBit

          prStderr result `shouldContain` "detect project"


        it "returns non-zero exit code" $ do
          result <- captureProcessResult nixShellBit

          prExitCode result `shouldBe` ExitFailure 1


      context "when VERSION cannot be detected" $
        before_ removeVersionFile $ do

        it "shows error" $ do
          result <- captureProcessResult nixShellBit

          prStderr result `shouldContain` "detect version"


        it "returns non-zero exit code" $ do
          result <- captureProcessResult nixShellBit

          prExitCode result `shouldBe` ExitFailure 1


        it "returns zero exit code if --list flag present" $ do
          result <- withArgs ["--list"]
                    (captureProcessResult nixShellBit)

          prExitCode result `shouldBe` ExitSuccess


      context "when NIX_SHELL_BIT_URL is not configured" $
        before_ removeConfigFile $ do

        let url = sandboxNixShellBitUrl sbx

            runWithInput xs = withStdin (C.unlines $ map C.pack xs)
                            . withArgs ["--list"]
                            $ captureProcessResult nixShellBit

        it "shows warning" $ do
          result <- runWithInput [ url, "yes" ]

          prStderr result `shouldContain` "NIX_SHELL_BIT_URL not found"


        it "prompts for NIX_SHELL_BIT_URL" $ do
          result <- runWithInput [url, "yes"]

          colorStrip (prStderr result) `shouldContain`
            "Please enter NIX_SHELL_BIT_URL"


        it "saves config by default" $ do
          _ <- runWithInput [url, ""]
          contents <- C.readFile =<< configPath

          contents `shouldContain` C.pack url


        it "lets user decline saving" $ do
          _ <- runWithInput [url, "no"]
          x <- doesFileExist =<< configPath

          x `shouldBe` False


        it "reprompts if answer is invalid" $ do
          result <- runWithInput [url, "fnord", "yes"]

          prStderr result `shouldMatch`
            "Save config.*Please answer y or n"

          x <- doesFileExist =<< configPath
          x `shouldBe` True


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
        before_ (setVersions []) $ do

        it "shows error" $ do
          result <- captureProcessResult nixShellBit

          prStderr result `shouldContain` "No versions"


        it "returns non-zero exit code" $ do
          result <- captureProcessResult nixShellBit

          prExitCode result `shouldBe` ExitFailure 1


      context "when VERSION is unavailable" $
        before_ (setVersions ["0.0.5", "0.0.6"]) $ do

        it "shows error" $ do
          result <- captureProcessResult nixShellBit

          prStderr result `shouldContain`
            C.pack (unVersion initialVersion <> " not found")


        it "returns non-zero exit code" $ do
          result <- captureProcessResult nixShellBit

          prExitCode result `shouldBe` ExitFailure 1


        it "lists available versions" $ do
          result <- captureProcessResult nixShellBit

          colorStrip (prStderr result) `shouldMatch`
            B.intercalate ".*" [ "\\<0\\.0\\.5\\>"
                               , "\\<0\\.0\\.6\\>"
                               ]


      context "when --list flag is present" $
        before_ (setVersions ["0.0.9", "0.1.0", "0.1.1"]) $ do

        it "lists available versions" $ do
          result <- withArgs ["--list"]
                    (captureProcessResult nixShellBit)

          colorStrip (prStdout result) `shouldMatch`
            B.intercalate ".*" [ "\\<0\\.0\\.9\\>"
                               , "\\<0\\.1\\.0\\>"
                               , "\\<0\\.1\\.1\\>"
                               ]


        it "returns zero exit code" $ do
          result <- withArgs ["--list"]
                    (captureProcessResult nixShellBit)

          prExitCode result `shouldBe` ExitSuccess


        it "does not warn about unavailable VERSION" $ do
          removeVersion sbx initialVersion
          result <- withArgs ["--list"]
                    (captureProcessResult nixShellBit)

          colorStrip (prStdout result) `shouldNotContain`
            C.pack (unVersion initialVersion <> " not found")
  where
    removeGitDir :: IO ()
    removeGitDir =
      removeDirectoryRecursive (sandboxLocalProject sbx </> ".git")

    removeVersionFile :: IO ()
    removeVersionFile =
      removeFile (sandboxLocalProject sbx </> "VERSION")

    removeConfigFile :: IO ()
    removeConfigFile =
      configPath >>= removeFile

    setVersions :: [String] -> IO ()
    setVersions vs =
      removeVersion sbx initialVersion >>
      traverse_ (addVersion sbx) vs

    colorStrip :: ByteString -> ByteString
    colorStrip = go ""
      where
        go acc "" = acc
        go acc xs = go (acc <> a) (dropSGR b)
          where
            -- SGR sequences begin with `ESC[` and end with `m`
            (a, b) = B.breakSubstring "\ESC[" xs
            dropSGR = B.drop 1 . B.dropWhile (/= 0x6d)

    shouldContain
      :: HasCallStack
      => ByteString
      -> ByteString
      -> Expectation
    shouldContain result expected =
      (expected `C.isInfixOf` result) `shouldBe` True

    shouldNotContain
      :: HasCallStack
      => ByteString
      -> ByteString
      -> Expectation
    shouldNotContain result unexpected =
      (unexpected `C.isInfixOf` result) `shouldBe` False

    shouldMatch
      :: ByteString
      -> ByteString
      -> Expectation
    shouldMatch result expected =
      (result =~ expected) `shouldBe` True
