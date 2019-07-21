module Shellbit.OperationSpec (spec) where

import Shellbit.Config    (configPath)
import Shellbit.Nix       (NixArguments(NixArguments))
import Shellbit.Operation (Operation(ExecuteShell, ListVersions),
                           OperationError(NoProject, NoVersion,
                           NoVersionsFound, VersionNotFound), operation)
import Shellbit.Options   (Options(Options), Command(Exec, List), Arg(Arg),
                           optProject, optVersion, optCommand, optArgs)
import Shellbit.Project   (Project(Project))
import Shellbit.Sbox      (localProject, initialVersion, projectName,
                           remoteNixShells, setVersions, xdgConfigPath)
import Shellbit.Version   (Version(Version))
import System.Directory   (doesFileExist, removeDirectoryRecursive, removeFile)
import System.Exit        (ExitCode(ExitFailure, ExitSuccess))
import System.FilePath    ((</>))
import Test.Hspec         (Spec, before_, context, describe, it, shouldBe,
                           shouldReturn, shouldThrow)
import Test.Utils         (capture, captureExitCode, prExitCode, silence,
                           withEnv, withInput)


spec :: FilePath -> Spec
spec sand =
  describe "operation" $ do
    let repo = localProject sand

        project = Project projectName

        version = Version initialVersion

        opts = Options
          { optProject = Just project
          , optVersion = Just version
          , optCommand = Just Exec
          , optArgs    = []
          }

    it "initializes config" $ do
      removeFile (xdgConfigPath sand </> "shellbit/config.dhall")

      _ <- withInput [remoteNixShells sand, "yes"]
         $ silence (operation opts)

      (configPath >>= doesFileExist) `shouldReturn` True


    context "when PROJECT cannot be detected automatically" $
      before_ (removeDirectoryRecursive (repo </> ".git")) $ do

      it "throws NoProject if options don't specify PROJECT" $ do
        let opts' = opts { optProject = Nothing }

        operation opts' `shouldThrow` (== NoProject)


      it "allows options to specify PROJECT" $ do
        ExecuteShell (NixArguments (_ : "--attr" : p : _)) <- operation opts

        p `shouldBe` projectName


    context "when VERSION cannot be detected automatically" $
      before_ (removeFile (repo </> "VERSION")) $ do

      it "throws NoVersion if options don't specify VERSION" $ do
        let opts' = opts { optVersion = Nothing }

        operation opts' `shouldThrow` (== NoVersion)


      it "allows options to specify VERSION" $ do
        (r, ExecuteShell (NixArguments _)) <- capture (operation opts)

        prExitCode r `shouldBe` ExitSuccess


    context "when no versions are available for PROJECT" $
      before_ (setVersions sand []) $

      it "throws NoVersionsFound" $
        operation opts `shouldThrow` (== NoVersionsFound project)


    context "when optCommand is List" $ do
      let opts' = opts { optCommand = Just List }
          versions = ["0.0.9", "0.1.0", "0.1.1"]

      it "returns a ListVersions operation" $ do
        setVersions sand versions

        operation opts' `shouldReturn`
          ListVersions (map Version versions) (Just version)


    context "when optCommand is ExecuteShell" $ do
      it "returns an ExecuteShell operation" $ do
        ExecuteShell (NixArguments (_:_:p:_)) <- operation opts

        p `shouldBe` projectName


      it "passes trailing arguments to nix-shell" $ do
        let opts' = opts
              { optArgs = [ Arg "--pure"
                          , Arg "--run"
                          , Arg "foo"
                          ]
              }

        ExecuteShell (NixArguments (_:_:_:args)) <- operation opts'

        args `shouldBe` ["--pure", "--run", "foo"]


    context "when VERSION is unavailable" $
      before_ (setVersions sand ["0.0.5", "0.0.6"]) $

      it "throws VersionNotFound" $
        operation opts `shouldThrow`
          (== VersionNotFound [Version "0.0.5", Version "0.0.6"] version)


    it "returns non-zero exit if SHELLBIT_URL cannot be read" $ do
      r <- withEnv
           [("SHELLBIT_URL", Just "/dev/null")]
           $ captureExitCode (operation opts)

      r `shouldBe` ExitFailure 1


    it "returns non-zero exit if SHELLBIT_BRANCH cannot be read" $ do
      r <- withEnv
           [("SHELLBIT_BRANCH", Just "fnord")]
           $ captureExitCode (operation opts)

      r `shouldBe` ExitFailure 1
