module Shellbit.MainSpec (spec) where

import Data.List          (intercalate)
import Shellbit.Main      (toOperation)
import Shellbit.Nix       (NixArguments(NixArguments))
import Shellbit.Operation (Operation(ExecuteShell))
import Shellbit.Options   (Options(Options), Command(Exec), optArgs, optCommand,
                           optProject, optVersion)
import Shellbit.Project   (Project(Project))
import Shellbit.Sbox      (initialVersion, localProject, projectName, setVersions)
import Shellbit.Version   (Version(Version))
import System.Directory   (removeDirectoryRecursive, removeFile)
import System.Exit        (ExitCode(ExitFailure, ExitSuccess))
import System.FilePath    ((</>))
import Test.Hspec         (Spec, before_, context, describe, it, shouldBe,
                           shouldContain)
import Test.Utils         (capture, captureExitCode, captureStderr, prExitCode,
                           shouldMatch, string)


spec :: FilePath -> Spec
spec sand =
  describe "toOperation" $ do
    let repo    = localProject sand
        project = Project projectName
        version = Version initialVersion

        opts = Options
          { optProject = Just project
          , optVersion = Just version
          , optCommand = Just Exec
          , optArgs    = []
          }

    it "can return an operation" $ do
      (r, ExecuteShell (NixArguments _)) <- capture (toOperation opts)

      prExitCode r `shouldBe` ExitSuccess


    context "when NoProject is thrown" $
      before_ (removeDirectoryRecursive (repo </> ".git")) $ do

        let opts' = opts { optProject = Nothing }

        it "shows error" $ do
          x <- captureStderr (toOperation opts')

          string x `shouldContain` "detect project"


        it "returns non-zero exit code" $ do
          e <- captureExitCode (toOperation opts')

          e `shouldBe` ExitFailure 1


    context "when NoVersion is thrown" $
      before_ (removeFile (repo </> "VERSION")) $ do

        let opts' = opts { optVersion = Nothing }

        it "shows error" $ do
          x <- captureStderr (toOperation opts')

          string x `shouldContain` "detect version"


        it "returns non-zero exit code" $ do
          e <- captureExitCode (toOperation opts')

          e `shouldBe` ExitFailure 1


    context "when NoVersionsFound is thrown" $
      before_ (setVersions sand []) $ do

        it "shows error" $ do
          x <- captureStderr (toOperation opts)

          string x `shouldContain` "No versions"


        it "returns non-zero exit code" $ do
          e <- captureExitCode (toOperation opts)

          e `shouldBe` ExitFailure 1


    context "when VersionNotFound is thrown" $
      before_ (setVersions sand ["0.0.5", "0.0.6"]) $ do

        it "shows error" $ do
          x <- captureStderr (toOperation opts)

          string x `shouldContain` (initialVersion ++ " not found")


        it "returns non-zero exit code" $ do
          e <- captureExitCode (toOperation opts)

          e `shouldBe` ExitFailure 1


        it "lists available versions" $ do
          x <- captureStderr (toOperation opts)

          string x `shouldMatch`
            intercalate ".*" [ "\\<0\\.0\\.5\\>"
                             , "\\<0\\.0\\.6\\>"
                             ]
