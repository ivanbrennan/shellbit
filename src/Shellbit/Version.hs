{-# LANGUAGE OverloadedStrings #-}

module Shellbit.Version
  ( Version(..)
  , currentVersion
  , detectVersion
  ) where

import Control.Applicative       ((<|>))
import Control.Monad             ((>=>))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Data.Aeson                (FromJSON, decodeFileStrict, parseJSON,
                                  withObject, (.:))
import Data.Maybe                (listToMaybe)
import Distribution.Package      (packageVersion)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.Pretty       (prettyShow)
import Distribution.Verbosity    (silent)
import Shellbit.Git              (gitDiscoverRepo)
import System.Directory          (getCurrentDirectory, getHomeDirectory)
import System.FilePath           (dropTrailingPathSeparator, takeDirectory)
import System.FilePath.Find      (FileType(RegularFile), FilterPredicate, depth,
                                  extension, fileName, fileType, find, (==?),
                                  (&&?))

import qualified Data.ByteString.Char8 as BS8


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
      MaybeT . findFile (extension ==? ".cabal")

    findPkgJSONFile :: FilePath -> MaybeT IO FilePath
    findPkgJSONFile =
      MaybeT . findFile (fileName ==? "package.json")

    findVersionFile :: FilePath -> MaybeT IO FilePath
    findVersionFile =
      MaybeT . findFile (fileName ==? "VERSION")

    findFile :: FilterPredicate -> FilePath -> IO (Maybe FilePath)
    findFile predicate =
      fmap listToMaybe
        . find (depth ==? 0) ((fileType ==? RegularFile) &&? predicate)

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
        line <- MaybeT (listToMaybe . BS8.lines <$> BS8.readFile path)
        pure $ Version (BS8.unpack line)
