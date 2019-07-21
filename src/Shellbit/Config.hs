{-# LANGUAGE OverloadedStrings #-}

module Shellbit.Config
  ( Config(..)
  , configInit
  , configPath
  ) where

import Control.Monad             (unless, when)
import Data.Foldable             (fold)
import Data.Semigroup            (Option(Option), Last(Last), getLast, getOption)
import Data.Text                 (Text)
import Data.Text.Prettyprint.Doc (unAnnotate)
import Dhall                     (InputType, Type, embed, field, inputField,
                                  inputFile, inputRecord, record, strictText,
                                  (>$<), (>*<))
import Dhall.Pretty              (prettyExpr)
import Shellbit.PPrint           (askSave, askUrl)
import System.Directory          (XdgDirectory(XdgConfig),
                                  createDirectoryIfMissing, doesFileExist,
                                  findFile, getXdgDirectory)
import System.Environment        (lookupEnv)
import System.FilePath           (takeDirectory, takeFileName, (</>), (<.>))

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Dhall


data Config = Config
  { cShellbitUrl    :: Text
  , cShellbitBranch :: Maybe Text
  } deriving Show


data Attrs = Attrs
  { shellbitUrl    :: Option (Last Text)
  , shellbitBranch :: Option (Last Text)
  } deriving Show

instance Semigroup Attrs where
  Attrs a b <> Attrs a' b' =
    Attrs (a <> a') (b <> b')

instance Monoid Attrs where
  mempty = Attrs mempty mempty


toMaybe :: Option (Last Text) -> Maybe Text
toMaybe =
  fmap getLast . getOption


toOption :: Maybe Text -> Option (Last Text)
toOption =
  Option . fmap Last


configInit :: IO Config
configInit =
  do
    config <- toConfig =<< fold [fromFile, fromEnv]
    exists <- doesFileExist =<< configPath
    unless exists (save config)
    pure config
  where
    fromFile :: IO Attrs
    fromFile =
      do
        path <- configPath
        file <- findFile [takeDirectory path] (takeFileName path)
        maybe (pure mempty) load file

    fromEnv :: IO Attrs
    fromEnv =
      Attrs <$> envOption "SHELLBIT_URL"
            <*> envOption "SHELLBIT_BRANCH"

    envOption :: String -> IO (Option (Last Text))
    envOption =
      fmap (toOption . fmap T.pack) . lookupEnv

    toConfig :: Attrs -> IO Config
    toConfig (Attrs url branch) =
      Config <$> maybe (T.pack <$> askUrl) pure (toMaybe url)
             <*> (pure . toMaybe) branch


configPath :: IO FilePath
configPath =
    (</> fileName) <$> directory
  where
    directory :: IO FilePath
    directory = getXdgDirectory XdgConfig package

    package :: String
    package = "shellbit"

    fileName :: FilePath
    fileName = "config" <.> "dhall"


load :: FilePath -> IO Attrs
load =
    inputFile attrs
  where
    attrs :: Type Attrs
    attrs =
      record ( Attrs <$> field "shellbitUrl"    opt
                     <*> field "shellbitBranch" opt )

    opt :: Type (Option (Last Text))
    opt =
      toOption <$> Dhall.maybe strictText


save :: Config -> IO ()
save config =
  do
    path    <- configPath
    confirm <- askSave path

    when confirm (write path)
  where
    write :: FilePath -> IO ()
    write path =
      createDirectoryIfMissing True (takeDirectory path) >>
      BS8.writeFile path (serialize attrs)

    attrs :: Attrs
    attrs = Attrs
      { shellbitUrl    = (toOption . Just . cShellbitUrl) config
      , shellbitBranch = (toOption . cShellbitBranch) config
      }


serialize :: Attrs -> BS8.ByteString
serialize =
    BS8.pack . show . unAnnotate . prettyExpr . embed input
  where
    input :: InputType Attrs
    input =
      inputRecord ( adapt >$< inputField "shellbitUrl"
                          >*< inputField "shellbitBranch" )

    adapt :: Attrs -> (Maybe Text, Maybe Text)
    adapt attrs =
      ( toMaybe (shellbitUrl    attrs)
      , toMaybe (shellbitBranch attrs)
      )
