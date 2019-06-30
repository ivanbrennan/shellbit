module Main (main) where

import NixShellBit.Sandbox (Sandbox, useSandbox, withSandbox)
import Test.Hspec          (Spec, around_, hspec)

import qualified NixShellBit.MainSpec


main :: IO ()
main =
  withSandbox $ \sbx ->
    hspec $ around_ (useSandbox sbx)
          $ spec sbx


spec :: Sandbox -> Spec
spec = NixShellBit.MainSpec.spec
