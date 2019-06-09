module NixShellBit.Options
  ( Args(..)
  , Options(..)
  , Command(..)
  , args
  ) where

import Data.Semigroup ((<>))
import Options.Applicative (Parser, command, help, info, long,
                            many, metavar, progDesc, short,
                            strOption, subparser, (<|>))


data Args = Args Options Command


data Options = Options
  { projects :: [Project]
  , versions :: [Version]
  }


data Command
  = Exec
  | List


type Project = String
type Version = String


args :: Parser Args
args = Args
    <$> ( Options
       <$> many project
       <*> many version
        )
    <*> ( subparser
           ( exec
          <> list
           )
       <|> pure Exec
        )
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

    exec = command "exec" $
      info (pure Exec) (progDesc "Enter project's nix-shell")

    list = command "list" $
      info (pure List) (progDesc "List available version(s)")
