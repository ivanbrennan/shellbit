module Main (main) where

import Data.Foldable        (traverse_)
import NixShellBit.Nix      (tmpClonePrefix)
import NixShellBit.Sbox     (localProject, withSandbox, xdgConfigPath)
import System.Directory     (removeDirectoryRecursive, withCurrentDirectory)
import System.FilePath.Find (FileType(Directory), depth, fileName, fileType,
                             find, (==?), (~~?), (&&?))
import System.IO.Temp       (getCanonicalTemporaryDirectory)
import Test.Hspec           (after_, before_, hspec)
import Test.Sandbox         (sbPath, refresh)
import Test.Utils           (withEnv)

import qualified NixShellBit.ColumnSpec    as ColumnSpec
import qualified NixShellBit.ConfigSpec    as ConfigSpec
import qualified NixShellBit.GitSpec       as GitSpec
import qualified NixShellBit.MainSpec      as MainSpec
import qualified NixShellBit.NixSpec       as NixSpec
import qualified NixShellBit.OperationSpec as OperationSpec
import qualified NixShellBit.OptionsSpec   as OptionsSpec
import qualified NixShellBit.PPrintSpec    as PPrintSpec
import qualified NixShellBit.ProjectSpec   as ProjectSpec
import qualified NixShellBit.VersionSpec   as VersionSpec


main :: IO ()
main =
    withSandbox $ \sbox ->
      let
        sand :: FilePath
        sand = sbPath sbox
      in
        withCurrentDirectory (localProject sand) $
          withEnv
            [("XDG_CONFIG_HOME", Just $ xdgConfigPath sand)]
            $ hspec $ before_ (refresh sbox) $ after_ removeTempClones $
            do
              ColumnSpec.spec
              ConfigSpec.spec sand
              GitSpec.spec sand
              MainSpec.spec sand
              NixSpec.spec sand
              OperationSpec.spec sand
              OptionsSpec.spec
              PPrintSpec.spec
              ProjectSpec.spec sand
              VersionSpec.spec sand
  where
    removeTempClones :: IO ()
    removeTempClones =
      findTempClones
        >>= traverse_ removeDirectoryRecursive

    findTempClones :: IO [FilePath]
    findTempClones =
      getCanonicalTemporaryDirectory
        >>= find (depth ==? 0)
                 ((fileName ~~? tmpClonePrefix ++ "-*") &&?
                  (fileType ==? Directory))
