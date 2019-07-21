module Shellbit.ConfigSpec (spec) where

import Shellbit.Config  (configInit, configPath, cShellbitUrl)
import Shellbit.Sbox    (remoteNixShells, xdgConfigPath)
import System.Directory (doesFileExist, removeFile)
import System.FilePath  ((</>))
import Test.Hspec       (Spec, before_, context, describe, it, shouldBe,
                         shouldContain, shouldNotContain)
import Test.Utils       (capture, prStderr, silence, string, withEnv, withInput)

import qualified Data.Text as T


spec :: FilePath -> Spec
spec sand = do
  describe "configInit" $ do
    it "uses configured SHELLBIT_URL" $ do
      c <- configInit

      cShellbitUrl c `shouldBe` T.pack (remoteNixShells sand)


    it "can be overridden by environment variable" $ do
      let override = "git@github.com:override/ooo.git"

      withEnv [("SHELLBIT_URL", Just override)] $ do
        c <- silence configInit

        cShellbitUrl c `shouldBe` T.pack override


    context "when config file does not exist" $ do
      let file = xdgConfigPath sand </> "shellbit/config.dhall"
          url  = "git@github.com:U/2.git"

      before_ (removeFile file) $ do

        it "prompts for SHELLBIT_URL if not set in environment" $ do
          (r, c) <- withInput [url, "yes"] (capture configInit)

          string (prStderr r) `shouldContain`
            "Please enter SHELLBIT_URL"

          cShellbitUrl c `shouldBe` T.pack url


        it "uses environment variable if set" $ do
          (r, c) <- withEnv [("SHELLBIT_URL", Just url)]
                  $ withInput ["yes"] (capture configInit)

          string (prStderr r) `shouldNotContain` "Please enter"

          cShellbitUrl c `shouldBe` T.pack url


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
        d `shouldBe` "/foo/shellbit/config.dhall"
