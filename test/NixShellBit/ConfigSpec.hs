module NixShellBit.ConfigSpec (spec) where

import NixShellBit.Config (configInit, configPath, cNixShellBitUrl)
import NixShellBit.Sbox   (remoteNixShells, xdgConfigPath)
import System.Directory   (doesFileExist, removeFile)
import System.FilePath    ((</>))
import Test.Hspec         (Spec, before_, context, describe, it, shouldBe,
                           shouldContain, shouldNotContain)
import Test.Main          (prStderr, withEnv)
import Test.Utils         (silence, withInput, capture, string)

import qualified Data.Text as T


spec :: FilePath -> Spec
spec sand = do
  describe "configInit" $ do
    it "uses configured NIX_SHELL_BIT_URL" $ do
      c <- configInit

      cNixShellBitUrl c `shouldBe` T.pack (remoteNixShells sand)


    it "can be overridden by environment variable" $ do
      let override = "git@github.com:override/ooo.git"

      withEnv [("NIX_SHELL_BIT_URL", Just override)] $ do
        c <- silence configInit

        cNixShellBitUrl c `shouldBe` T.pack override


    context "when config file does not exist" $ do
      let file = xdgConfigPath sand </> "nix-shell-bit/config.dhall"
          url  = "git@github.com:U/2.git"

      before_ (removeFile file) $ do

        it "prompts for NIX_SHELL_BIT_URL if not set in environment" $ do
          (r, c) <- withInput [url, "yes"] (capture configInit)

          string (prStderr r) `shouldContain`
            "Please enter NIX_SHELL_BIT_URL"

          cNixShellBitUrl c `shouldBe` T.pack url


        it "uses environment variable if set" $ do
          (r, c) <- withEnv [("NIX_SHELL_BIT_URL", Just url)]
                  $ withInput ["yes"] (capture configInit)

          string (prStderr r) `shouldNotContain` "Please enter"

          cNixShellBitUrl c `shouldBe` T.pack url


        it "saves config if user confirms" $ do
          _ <- withInput [url, "yes"] (silence configInit)
          c <- readFile =<< configPath

          c `shouldContain` url


        it "skips saving if user declines" $ do
          _ <- withInput [url, "no"] (silence configInit)
          f <- doesFileExist =<< configPath

          f `shouldBe` False


  describe "configPath" $
    it "uses XDG_CONFIG_HOME" $
      withEnv [("XDG_CONFIG_HOME", Just "/foo")] $ do
        d <- configPath
        d `shouldBe` "/foo/nix-shell-bit/config.dhall"
