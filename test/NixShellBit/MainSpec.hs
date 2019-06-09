module NixShellBit.MainSpec (spec) where

import NixShellBit.Main (nixShellBit)
import System.IO.Silently (capture_)
import Test.Hspec (Spec, describe, it, shouldReturn)


spec :: Spec
spec = describe "nixShellBit" $
  it "displays parsed options" $
    capture_ nixShellBit `shouldReturn`
      "Cmd {project = Nothing, version = Nothing, list = False}\n"
