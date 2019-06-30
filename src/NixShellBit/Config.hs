{-# LANGUAGE DeriveGeneric #-}

module NixShellBit.Config
  ( Config(..)
  , configInit
  , configPath
  ) where

import Control.Monad             (unless, when)
import Data.Foldable             (fold)
import Data.Text                 (Text)
import Data.Text.Prettyprint.Doc (unAnnotate)
import Dhall                     (Generic, Inject, Interpret, auto, embed,
                                  inject, inputFile)
import Dhall.Pretty              (prettyExpr)
import NixShellBit.PPrint        (askSave, askUrl)
import System.Directory          (XdgDirectory(XdgConfig),
                                  createDirectoryIfMissing, doesFileExist,
                                  findFile, getXdgDirectory)
import System.Environment        (lookupEnv)
import System.FilePath           (takeDirectory, takeFileName, (</>), (<.>))

import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T


data Config = Config
  { nixShellBitUrl    :: Text
  , nixShellBitBranch :: Maybe Text
  } deriving (Generic, Show)


data Attrs = Attrs
  { aNixShellBitUrl    :: Maybe Text
  , aNixShellBitBranch :: Maybe Text
  } deriving (Generic, Show)

instance Interpret Attrs
instance Inject Attrs

instance Semigroup Attrs where
  Attrs a b <> Attrs a' b' =
    Attrs (a <> a') (b <> b')

instance Monoid Attrs where
  mempty = Attrs Nothing Nothing


configInit :: IO Config
configInit =
  do
    config <- toConfig =<< fold [fromEnv, fromFile]
    exists <- doesFileExist =<< configPath
    unless exists (save config)
    pure config
  where
    fromEnv :: IO Attrs
    fromEnv =
      Attrs <$> envText "NIX_SHELL_BIT_URL"
            <*> envText "NIX_SHELL_BIT_BRANCH"

    fromFile :: IO Attrs
    fromFile =
      do
        path <- configPath
        file <- findFile [takeDirectory path] (takeFileName path)
        maybe (pure mempty) (inputFile auto) file

    toConfig :: Attrs -> IO Config
    toConfig a =
      Config <$> maybe (T.pack <$> askUrl) pure (aNixShellBitUrl a)
             <*> pure (aNixShellBitBranch a)

    envText :: String -> IO (Maybe Text)
    envText = (fmap . fmap) T.pack . lookupEnv


configPath :: IO FilePath
configPath =
    (</> fileName) <$> directory
  where
    directory :: IO FilePath
    directory = getXdgDirectory XdgConfig package

    package :: String
    package = "nix-shell-bit"

    fileName :: FilePath
    fileName = "config" <.> "dhall"


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
      C.writeFile path (serialize attrs)

    attrs :: Attrs
    attrs = Attrs
      { aNixShellBitUrl    = Just (nixShellBitUrl config)
      , aNixShellBitBranch = nixShellBitBranch config
      }

    serialize :: Attrs -> C.ByteString
    serialize =
      C.pack . show . unAnnotate . prettyExpr . embed inject
