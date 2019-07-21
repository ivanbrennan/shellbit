module Shellbit.VersionSpec (spec) where

import Shellbit.Sbox    (fixturesPath, localProject)
import Shellbit.Version (Version(Version), currentVersion, detectVersion)
import System.Directory (createDirectoryIfMissing, copyFile,
                         removeDirectoryRecursive, withCurrentDirectory)
import System.FilePath  ((</>))
import Test.Hspec       (Spec, around_, before_, context, describe, it,
                         shouldReturn)


spec :: FilePath -> Spec
spec sand = do
  let repo = localProject sand
      cabalFile   = fixturesPath sand </> "PROJECT.cabal"
      pkgJSONFile = fixturesPath sand </> "package.json"

  describe "currentVersion" $ do
    let path = repo </> "foo/bar"

    around_ ( \io -> do
              createDirectoryIfMissing True path
              copyFile pkgJSONFile (path </> "package.json")
              withCurrentDirectory path io ) $ do

      context "when working directory is in a git repo" $
        it "detects version from repo's top-level directory" $
          currentVersion `shouldReturn` Just (Version "0.1.0")

      context "when working directory is not in a git repo" $
        before_ (removeDirectoryRecursive (repo </> ".git")) $

        it "detects version from current working directory" $
          currentVersion `shouldReturn` Just (Version "1.2.3")


  describe "detectVersion" $ do
    it "can parse version from a generic VERSION file" $
      detectVersion repo `shouldReturn` Just (Version "0.1.0")

    it "can parse version from a cabal file" $ do
      copyFile cabalFile (repo </> "PROJECT.cabal")

      detectVersion repo `shouldReturn` Just (Version "0.1.0.0")

    it "can parse version from a package.json file" $ do
      copyFile pkgJSONFile (repo </> "package.json")

      detectVersion repo `shouldReturn` Just (Version "1.2.3")
