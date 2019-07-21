module Shellbit.ProjectSpec (spec) where

import Distribution.Simple.Utils (copyDirectoryRecursive)
import Distribution.Verbosity    (silent)
import Shellbit.Git              (git_)
import Shellbit.Project          (Project(Project), currentProject, detectProject)
import Shellbit.Sbox             (localProject, remoteProject)
import System.Directory          (removeDirectoryRecursive)
import System.FilePath           (takeBaseName, (</>))
import Test.Hspec                (Spec, before_, context, describe, it, shouldReturn)

spec :: FilePath -> Spec
spec sand = do
  let repo   = localProject sand
      origin = remoteProject sand

  describe "currentProject" $ do
    context "when working directory is in a git repo" $
      it "can detect project from repo's remote" $
        currentProject `shouldReturn`
          (Just . Project . takeBaseName) origin

    context "when working directory is not in a git repo" $
      before_ (removeDirectoryRecursive (repo </> ".git")) $

      it "returns Nothing" $
        currentProject `shouldReturn` Nothing


  describe "detectProject" $ do
    it "returns origin's basename" $
      detectProject repo `shouldReturn`
        (Just . Project . takeBaseName) origin

    context "when repo has remotes other than origin" $ do
      let fork = sand </> "fork.xxx"

      before_
        ( copyDirectoryRecursive silent origin fork >>
          git_ ["-C", repo, "remote", "add", "fork", fork]
        ) $ do

        it "prefers origin" $
          detectProject repo `shouldReturn`
            (Just . Project . takeBaseName) origin

        it "uses other remote if there is no \"origin\" remote" $ do
          git_ ["-C", repo, "remote", "remove", "origin"]

          detectProject repo `shouldReturn`
            (Just . Project . takeBaseName) fork

    context "when repo has no remotes" $
      before_ (git_ ["-C", repo, "remote", "remove", "origin"]) $

        it "returns Nothing" $
          detectProject repo `shouldReturn` Nothing
