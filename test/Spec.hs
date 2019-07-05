module Main (main) where

import NixShellBit.Sbox (localProject, withSandbox, xdgConfigPath)
import System.Directory (withCurrentDirectory)
import Test.Hspec       (before_, hspec)
import Test.Main        (withEnv)
import Test.Sandbox     (sbPath, refresh)

import qualified NixShellBit.ConfigSpec  as ConfigSpec
import qualified NixShellBit.GitSpec     as GitSpec
import qualified NixShellBit.MainSpec    as MainSpec
import qualified NixShellBit.NixSpec     as NixSpec
import qualified NixShellBit.OptionsSpec as OptionsSpec
import qualified NixShellBit.PPrintSpec  as PPrintSpec
import qualified NixShellBit.ProjectSpec as ProjectSpec
import qualified NixShellBit.VersionSpec as VersionSpec


main :: IO ()
main =
  withSandbox $ \sbox ->
    let
      sand :: FilePath
      sand = sbPath sbox
    in
      withCurrentDirectory (localProject sand) $
        withEnv
          [("XDG_CONFIG_HOME", Just $ xdgConfigPath sand)]
          $ hspec $ before_ (refresh sbox) $
          do
            ConfigSpec.spec sand
            GitSpec.spec sand
            MainSpec.spec sand
            NixSpec.spec sand
            OptionsSpec.spec
            PPrintSpec.spec
            ProjectSpec.spec sand
            VersionSpec.spec sand
