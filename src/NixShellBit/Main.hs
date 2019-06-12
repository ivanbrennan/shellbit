module NixShellBit.Main
  ( nixShellBit
  ) where

import Data.Maybe          (fromMaybe)
import NixShellBit.Options (Options, Project(Project), Version(Version),
                            Command(Exec), options, optProject,
                            optVersion, optCommand, optArgs)
import Options.Applicative (briefDesc, execParser, info, helper)


nixShellBit :: IO ()
nixShellBit =
    run =<< execParser o
  where
    o = info (helper <*> options) briefDesc


run :: Options -> IO ()
run opts = do
    print (project opts)
    print (version opts)
    print (command opts)
    print (optArgs opts)
  where
    project = fromMaybe (Project "currentProject") . optProject
    version = fromMaybe (Version "currentVersion") . optVersion
    command = fromMaybe Exec . optCommand
