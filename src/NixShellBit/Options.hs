module NixShellBit.Options
  ( Command(..)
  , Options(..)
  , options
  ) where

import NixShellBit.Project (Project(Project))
import NixShellBit.Version (Version(Version))
import Options.Applicative (Parser, action, flag', help, long, many,
                            metavar, short, strArgument, strOption)


data Options = Options
  { optProject :: Maybe Project
  , optVersion :: Maybe Version
  , optCommand :: Maybe Command
  , optArgs    :: [Arg]
  } deriving Show


data Command
  = Exec
  | List
  deriving Show


newtype Arg
  = Arg String
  deriving Show


options :: Parser Options
options =
  Options
    <$> (maybeLast <$> many (Project <$> project))
    <*> (maybeLast <$> many (Version <$> version))
    <*> (maybeLast <$> many cmd)
    <*> many (Arg <$> arg)
  where
    project = strOption
      ( long "project"
     <> short 'p'
     <> metavar "PROJECT"
     <> help "Use PROJECT instead of the current project"
      )
    version = strOption
      ( long "version"
     <> short 'v'
     <> metavar "VERSION"
     <> help "Use VERSION instead of the current version"
      )
    cmd = flag' List
      ( long "list"
     <> short 'l'
     <> help "List available version(s)"
      )
    arg = strArgument
      ( metavar "ARG..."
     <> help "Args to pass to nix-shell"
     <> action "file"
      )

    maybeLast [] = Nothing
    maybeLast xs = Just (last xs)
