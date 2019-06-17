module NixShellBit.Main
  ( nixShellBit
  ) where

import Data.Maybe          (fromMaybe)
import Data.Version        (showVersion)
import NixShellBit.PPrint  (text, vcat, yellow, line, die)
import NixShellBit.Options (Options, Command(Exec), options, optProject,
                            optVersion, optCommand, optArgs)
import NixShellBit.Project (Project, detectProject)
import NixShellBit.Version (Version, detectVersion)
import Options.Applicative (briefDesc, execParser, info, infoOption,
                            helper, hidden, short)

import qualified Paths_nix_shell_bit as Self


nixShellBit :: IO ()
nixShellBit =
    run =<< execParser o
  where
    o = info (helper <*> v <*> options) briefDesc
    v = infoOption (showVersion Self.version) (short 'V' <> hidden)


run :: Options -> IO ()
run opts =
  do
    project >>= maybe oopsNoProject print
    version >>= maybe oopsNoVersion print
    print (command opts)
    print (optArgs opts)
  where
    project :: IO (Maybe Project)
    project =
      case optProject opts of
        Just pj -> pure (Just pj)
        Nothing -> detectProject

    version :: IO (Maybe Version)
    version =
      case optVersion opts of
        Just vn -> pure (Just vn)
        Nothing -> detectVersion

    command = fromMaybe Exec . optCommand


oopsNoProject :: IO a
oopsNoProject =
  die $ vcat
      [ yellow (text "Could not detect project")
      , text "Try --project=PROJECT"
      , line
      ]


oopsNoVersion :: IO a
oopsNoVersion =
  die $ vcat
      [ yellow (text "Could not detect version")
      , text "Try --version=VERSION"
      , line
      ]
