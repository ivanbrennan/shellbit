module Main (main) where

import NixShellBit.Sbox (withSandbox)
import System.Directory (withCurrentDirectory)
import System.FilePath  ((</>))
import Test.Hspec       (before_, hspec)
import Test.Main        (withEnv)
import Test.Sandbox     (sbPath, refresh)

import qualified NixShellBit.ConfigSpec  as ConfigSpec
import qualified NixShellBit.GitSpec     as GitSpec
import qualified NixShellBit.MainSpec    as MainSpec
import qualified NixShellBit.OptionsSpec as OptionsSpec
import qualified NixShellBit.PPrintSpec  as PPrintSpec


main :: IO ()
main =
  withSandbox $ \sbox ->
    let
      sand :: FilePath
      sand = sbPath sbox
    in
      withCurrentDirectory (sand </> "local/PROJECT") $
        withEnv
          [("XDG_CONFIG_HOME", Just $ sand </> "XDG_CONFIG_HOME")]
          $ hspec $ before_ (refresh sbox) $
          do
            ConfigSpec.spec sand
            GitSpec.spec sand
            MainSpec.spec sand
            OptionsSpec.spec
            PPrintSpec.spec
