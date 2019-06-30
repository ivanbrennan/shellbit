module Main (main) where

import NixShellBit.Sandbox (Sandbox, useSandbox, withSandbox)
import Test.Hspec          (Spec, around_, hspec)

import qualified NixShellBit.MainSpec as MainSpec
import qualified NixShellBit.OptionsSpec as OptionsSpec


main :: IO ()
main =
  withSandbox $ \sbx ->
    hspec $ around_ (useSandbox sbx)
          $ spec sbx


spec :: Sandbox -> Spec
spec sbx =
  do
    MainSpec.spec sbx
    OptionsSpec.spec
