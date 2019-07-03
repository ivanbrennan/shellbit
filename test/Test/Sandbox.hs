module Test.Sandbox
  ( Sandbox
  , sbPath
  , refresh
  , withSandbox
  ) where

import Distribution.Simple.Utils (copyDirectoryRecursive)
import Distribution.Verbosity    (silent)
import System.Directory          (createDirectory, removeDirectoryRecursive,
                                  withCurrentDirectory)
import System.FilePath           ((</>))
import System.IO.Temp            (withSystemTempDirectory)


data Sandbox = Sandbox Seed FilePath
newtype Seed = Seed FilePath


sbPath
  :: Sandbox
  -> FilePath
sbPath (Sandbox _ path)
  = path


withSandbox
  :: (FilePath -> IO ())
  -> (Sandbox -> IO ())
  -> IO ()
withSandbox initialize action =
  withSystemTempDirectory "test-sandbox" $ \tmp ->
    let
      seed = tmp </> "seed"
      sand = tmp </> "sand"
    in
      do
        createDirectory sand
        initialize sand
        copyDirectoryRecursive silent sand seed
        action (Sandbox (Seed seed) sand)


refresh
  :: Sandbox
  -> IO ()
refresh (Sandbox (Seed seed) sand) =
  withCurrentDirectory seed $
    do
      removeDirectoryRecursive sand
      copyDirectoryRecursive silent seed sand
