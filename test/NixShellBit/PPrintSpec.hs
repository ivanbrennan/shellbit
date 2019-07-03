module NixShellBit.PPrintSpec (spec) where

import Data.List          (intercalate)
import NixShellBit.PPrint (askSave, askUrl, listItems)
import Test.Hspec         (Spec, describe, it, shouldBe, shouldContain)
import Test.Main          (prStderr)
import Test.Utils         (capture, captureStdout, captureStderr, shouldMatch,
                           silence, string, withInput)


spec :: Spec
spec = do
    describe "askUrl" $ do
      let url :: String
          url = "git@github.com:Foo/nix-shells.git"

      it "prompts for NIX_SHELL_BIT_URL" $ do
        e <- withInput [url] (captureStderr askUrl)

        string e `shouldContain` "Please enter NIX_SHELL_BIT_URL"


      it "returns the provided input" $ do
        u <- withInput [url] (silence askUrl)

        u `shouldBe` url


    describe "askSave" $ do
      let path :: FilePath
          path = "path/to/file"

      it "asks for confirmation" $ do
        e <- withInput ["yes"] (captureStderr (askSave path))

        string e `shouldContain` "Save config?"


      it "recognizes positive responses" $ do
        ts <- traverse (`withInput` silence (askSave path))
              [["yes"], ["y"], ["yES"]]

        ts `shouldBe` [True, True, True]


      it "recognizes negative responses" $ do
        fs <- traverse (`withInput` silence (askSave path))
              [["no"], ["n"], ["NO"]]

        fs `shouldBe` [False, False, False]


      it "defaults empty response to True" $ do
        t <- withInput [""] (silence (askSave path))

        t `shouldBe` True


      it "reprompts if response is invalid" $ do
        (r, t) <- withInput ["1", "2", "yes"]
                  (capture (askSave path))

        t `shouldBe` True

        string (prStderr r) `shouldMatch`
          intercalate ".*" [ "Please answer y or n"
                           , "Please answer y or n"
                           ]


    describe "listItems" $
      it "lists items on stdout" $ do
        o <- captureStdout (listItems ["fnord", "zoo"] Nothing)

        string o `shouldMatch` "fnord.*zoo"
