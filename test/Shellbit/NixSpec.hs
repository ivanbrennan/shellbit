{-# LANGUAGE OverloadedStrings #-}

module Shellbit.NixSpec (spec) where

import Shellbit.Nix     (NixDerivation(NixDerivation),
                            NixArguments(NixArguments), arguments, derivation)
import Shellbit.Options (Arg(Arg))
import Shellbit.Project (Project(Project), unProject)
import Shellbit.Sbox    (initialVersion, projectName, remoteNixShells)
import Shellbit.Version (Version(Version))
import System.Directory    (doesDirectoryExist)
import Test.Hspec          (Spec, describe, it, shouldBe, shouldReturn)

import qualified Data.Text as T


spec :: FilePath -> Spec
spec sand = do
  describe "derivation" $ do
    let project = Project projectName
        version = Version initialVersion

    it "references an archive url when possible" $ do
      let url     = "git@github.com:Foo/nix-shells.git"
          branch  = Nothing
          archive =
            "https://github.com/Foo/nix-shells/archive/"
            ++ projectName ++ "-" ++ initialVersion
            ++ ".tar.gz"

      derivation url branch project version `shouldReturn`
        NixDerivation archive

    it "can reference a specific branch" $ do
      let url     = "git@github.com:Foo/nix-shells.git"
          branch  = Just "master"
          archive =
            "https://github.com/Foo/nix-shells/archive/master.tar.gz"

      derivation url branch project version `shouldReturn`
        NixDerivation archive

    it "uses a temporary clone when archive can't be determined" $ do
      let url    = T.pack (remoteNixShells sand)
          branch = Nothing

      NixDerivation drv <- derivation url branch project version

      doesDirectoryExist drv `shouldReturn` True


  describe "arguments" $
    it "returns arguments suitable for passing to nix-shell" $ do
      let drv = "https://github.com/Foo/nix-shells/archive/master.tar.gz"
          project = Project projectName
          args = [Arg "foo", Arg "bar"]

      arguments (NixDerivation drv) project args `shouldBe`
        NixArguments [ drv
                     , "--attr"
                     , unProject project
                     , "foo"
                     , "bar"
                     ]
