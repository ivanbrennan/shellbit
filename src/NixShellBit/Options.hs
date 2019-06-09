module NixShellBit.Options
  ( CmdLine(..)
  , Options(..)
  , cmdline
  ) where

import Data.Semigroup ((<>))
import Options.Applicative (Parser, command, help, info, long,
                            many, metavar, progDesc, short,
                            strOption, subparser, (<|>))


data CmdLine
  = Exec Options
  | List Options
  deriving Show


data Options
  = Options [Project] [Version]
  deriving Show


newtype Project
  = Project String
  deriving Show


newtype Version
  = Version String
  deriving Show


cmdline :: Parser CmdLine
cmdline =
    subparser (exec <> list)
   <|>
    (defaultCommand <$> opts)
  where
    exec = command "exec" $
      info (Exec <$> opts) (progDesc "Enter project's nix-shell")

    list = command "list" $
      info (List <$> opts) (progDesc "List available version(s)")

    defaultCommand = Exec


opts :: Parser Options
opts = Options
    <$> many (Project <$> project)
    <*> many (Version <$> version)
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
