module NixShellBit.Main
  ( nixShellBit
  ) where

import NixShellBit.Options (Args(Args), Command(Exec, List),
                            args, projects, versions)
import Options.Applicative (execParser, info, helper, fullDesc, (<**>))


nixShellBit :: IO ()
nixShellBit =
    run =<< execParser opts
  where
    opts = info (args <**> helper) fullDesc


run :: Args -> IO ()
run (Args opts cmd) = do
    putStrLn $ "command: " ++ showCmd
    putStrLn $ "project: " ++ show (mLast (projects opts))
    putStrLn $ "version: " ++ show (mLast (versions opts))
  where
    showCmd =
      case cmd of
        Exec -> "exec"
        List -> "list"

    mLast xs =
      if null xs
      then Nothing
      else Just (last xs)
