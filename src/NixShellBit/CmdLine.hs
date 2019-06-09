{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module NixShellBit.CmdLine
  ( getCmd
  ) where

import System.Console.CmdArgs (Data, Typeable, cmdArgs, explicit, help,
                               name, program, typ, versionArg, (&=))


data Cmd = Cmd
  { project :: Maybe String
  , version :: Maybe String
  , list    :: Bool
  } deriving (Show, Data, Typeable)


cmd :: Cmd
cmd = Cmd
  { project = Nothing
      &= help "Use PROJECT instead of the current project"
      &= typ "PROJECT"
  , version = Nothing
      &= help "Use VERSION instead of the current version"
      &= typ "VERSION"
  , list = False
      &= help "List available version(s)"
  }
  &= versionArg [explicit, name "V"]
  &= program "nix-shell-bit"


getCmd :: IO Cmd
getCmd = cmdArgs cmd
