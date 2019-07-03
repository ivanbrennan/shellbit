module NixShellBit.ConfigSpec (spec) where

import NixShellBit.Config (configInit, configPath, cNixShellBitUrl)
import System.Directory   (doesFileExist, removeFile)
import System.FilePath    (takeDirectory, (</>))
import Test.Hspec         (Spec, before_, context, describe, it, shouldBe,
                           shouldContain, shouldNotContain)
import Test.Main          (prStderr, withEnv)
import Test.Utils         (silence, withInput, capture, string)

import qualified Data.Text as T


spec :: FilePath -> Spec
spec sand = do
  describe "configInit" $ do
    let url  = sand </> "remote/NIX_SHELLS"
        url2 = "git@github.com:U/2.git"

    it "uses configured NIX_SHELL_BIT_URL" $ do
      c <- configInit

      cNixShellBitUrl c `shouldBe` T.pack url


    -- it "can be overridden by environment variable" $ do
    --   withEnv [("NIX_SHELL_BIT_URL", Just url2)] $ do
    --     c <- silence configInit

    --     cNixShellBitUrl c `shouldBe` T.pack url2


    context "when config file does not exist" $
      before_ (removeFile (sand </> "XDG_CONFIG_HOME/nix-shell-bit/config.dhall")) $ do

      it "prompts for NIX_SHELL_BIT_URL if not set in environment" $ do
        (r, c) <- withInput [url2, "yes"] (capture configInit)

        string (prStderr r) `shouldContain`
          "Please enter NIX_SHELL_BIT_URL"

        cNixShellBitUrl c `shouldBe` T.pack url2


      it "uses environment variable if set" $ do
        (r, c) <- withEnv [("NIX_SHELL_BIT_URL", Just url2)]
                  ( withInput ["yes"] (capture configInit) )

        string (prStderr r) `shouldNotContain` "Please enter"

        cNixShellBitUrl c `shouldBe` T.pack url2


      it "saves config if user confirms" $ do
        _ <- withInput [url2, "yes"] (silence configInit)
        c <- readFile =<< configPath

        c `shouldContain` url2


      it "skips saving if user declines" $ do
        _ <- withInput [url2, "no"] (silence configInit)
        f <- doesFileExist =<< configPath

        f `shouldBe` False


  describe "configPath" $
    it "uses XDG_CONFIG_HOME" $
      withEnv [("XDG_CONFIG_HOME", Just "/foo")] $ do
        d <- takeDirectory <$> configPath
        d `shouldBe` "/foo/nix-shell-bit"
