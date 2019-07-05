{-# LANGUAGE OverloadedStrings #-}

module NixShellBit.Version
  ( Version(..)
  , currentVersion
  , detectVersion
  ) where

import Control.Applicative       ((<|>))
import Control.Monad             ((>=>))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Data.Aeson                (FromJSON, decodeFileStrict, parseJSON,
                                  withObject, (.:))
import Data.Maybe                (listToMaybe, maybe)
import Distribution.Package      (packageVersion)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.Pretty       (prettyShow)
import Distribution.Verbosity    (silent)
import NixShellBit.Git           (gitDiscoverRepo)
import System.Directory          (getCurrentDirectory, getHomeDirectory)
import System.FilePath           (dropTrailingPathSeparator, takeDirectory)
import System.FilePath.Find      (FileType(RegularFile), depth, extension,
                                  fileName, fileType, find, (==?), (&&?))

import qualified Data.ByteString.Char8 as C


newtype Version = Version
  { unVersion :: String
  } deriving (Eq, Show)

instance FromJSON Version where
  parseJSON = withObject "Object"
    (\o -> Version <$> o .: "version")


currentVersion :: IO (Maybe Version)
currentVersion =
  do
    startPath   <- getCurrentDirectory
    ceilingDirs <- pure <$> getHomeDirectory
    gitDir      <- gitDiscoverRepo startPath ceilingDirs

    detectVersion (maybe startPath parent gitDir)
  where
    parent :: FilePath -> FilePath
    parent =
      takeDirectory . dropTrailingPathSeparator


detectVersion :: FilePath -> IO (Maybe Version)
detectVersion directory =
    runMaybeT
      ( fromCabalPkg directory
     <|> fromNodePkg directory
     <|> fromGenericPkg directory
      )
  where
    fromCabalPkg :: FilePath -> MaybeT IO Version
    fromCabalPkg =
      findCabalFile >=> cabalPkgVersion

    fromNodePkg :: FilePath -> MaybeT IO Version
    fromNodePkg =
      findPkgJSONFile >=> nodePkgVersion

    fromGenericPkg :: FilePath -> MaybeT IO Version
    fromGenericPkg =
      findVersionFile >=> genericPkgVersion

    findCabalFile :: FilePath -> MaybeT IO FilePath
    findCabalFile =
      MaybeT
        . fmap listToMaybe
        . find (depth ==? 0)
               ((extension ==? ".cabal") &&?
                (fileType ==? RegularFile))

    findPkgJSONFile :: FilePath -> MaybeT IO FilePath
    findPkgJSONFile =
      MaybeT
        . fmap listToMaybe
        . find (depth ==? 0)
               ((fileName ==? "package.json") &&?
                (fileType ==? RegularFile))

    findVersionFile :: FilePath -> MaybeT IO FilePath
    findVersionFile =
      MaybeT
        . fmap listToMaybe
        . find (depth ==? 0)
               ((fileName ==? "VERSION") &&?
                (fileType ==? RegularFile))

    cabalPkgVersion :: FilePath -> MaybeT IO Version
    cabalPkgVersion =
      MaybeT
        . fmap (Just . Version . prettyShow . packageVersion)
        . readGenericPackageDescription silent

    nodePkgVersion :: FilePath -> MaybeT IO Version
    nodePkgVersion =
      MaybeT . decodeFileStrict

    genericPkgVersion :: FilePath -> MaybeT IO Version
    genericPkgVersion path =
      do
        line <- MaybeT (listToMaybe . C.lines <$> C.readFile path)
        pure $ Version (C.unpack line)
