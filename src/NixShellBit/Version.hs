{-# LANGUAGE OverloadedStrings #-}

module NixShellBit.Version
  ( Version(..)
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

import qualified Data.ByteString.Char8 as C8


newtype Version = Version
  { unVersion :: String
  } deriving Show

instance FromJSON Version where
  parseJSON = withObject "Object"
    (\o -> Version <$> o .: "version")


detectVersion :: IO (Maybe Version)
detectVersion =
  do
    startPath   <- getCurrentDirectory
    ceilingDirs <- pure <$> getHomeDirectory

    dir <- maybe startPath parentDirectory
             <$> gitDiscoverRepo startPath ceilingDirs

    fromCabalPkg dir
      <|> fromNodePkg dir
      <|> fromGenericPkg dir
  where
    parentDirectory :: FilePath -> FilePath
    parentDirectory =
      takeDirectory . dropTrailingPathSeparator

    fromCabalPkg :: FilePath -> IO (Maybe Version)
    fromCabalPkg =
      runMaybeT . (findCabalFile >=> cabalPkgVersion)

    fromNodePkg :: FilePath -> IO (Maybe Version)
    fromNodePkg =
      runMaybeT . (findPkgJSONFile >=> nodePkgVersion)

    fromGenericPkg :: FilePath -> IO (Maybe Version)
    fromGenericPkg =
      runMaybeT . (findVersionFile >=> genericPkgVersion)

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
        line <- MaybeT (listToMaybe . C8.lines <$> C8.readFile path)
        pure $ Version (C8.unpack line)
