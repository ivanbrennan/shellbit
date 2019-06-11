module NixShellBit.Main
  ( nixShellBit
  ) where

import NixShellBit.Options (CmdLine(Exec, List), Options, Project,
                            Version, args, cmdline, projects, versions)
import Options.Applicative (briefDesc, execParser, info, helper)


nixShellBit :: IO ()
nixShellBit =
    run =<< execParser o
  where
    o = info (helper <*> cmdline) briefDesc


run :: CmdLine -> IO ()
run (Exec opts) = do
  print (project opts)
  print (version opts)
  print (args opts)
run (List opts) = do
  print (project opts)
  print (version opts)
  print (args opts)


project :: Options -> Maybe Project
project opts = maybeLast (projects opts)


version :: Options -> Maybe Version
version opts = maybeLast (versions opts)


maybeLast :: [a] -> Maybe a
maybeLast [] = Nothing
maybeLast xs = Just (last xs)
