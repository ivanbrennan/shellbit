{-# LANGUAGE DeriveGeneric #-}

module NixShellBit.Config
  ( Config(..)
  , configPath
  , getConfig
  , saveConfig
  ) where

import Control.Monad      (when)
import Data.Foldable      (fold)
import Data.Text          (Text)
import Data.Text.Prettyprint.Doc (unAnnotate)
import Dhall              (Generic, Inject, Interpret,
                           auto, embed, inject, inputFile)
import Dhall.Pretty       (prettyExpr)
import NixShellBit.PPrint (askSave, askUrl)
import System.Directory   (XdgDirectory(XdgConfig),
                           createDirectoryIfMissing,
                           findFile, getXdgDirectory)
import System.Environment (lookupEnv)
import System.FilePath    (takeDirectory, takeFileName, (</>), (<.>))

import qualified Data.ByteString.Char8 as C8
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


configPath :: IO FilePath
configPath =
    (</> fileName) <$> directory
  where
    directory :: IO FilePath
    directory = getXdgDirectory XdgConfig package

    package :: String
    package = "nix-shell-bit"

    fileName :: FilePath
    fileName = package <.> "dhall"


getConfig :: IO Config
getConfig =
    fold [fromEnv, fromFile] >>= toConfig
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


saveConfig :: Config -> IO ()
saveConfig config =
  do
    path    <- configPath
    confirm <- askSave path

    when confirm (writeConfig path)
  where
    writeConfig :: FilePath -> IO ()
    writeConfig path =
      createDirectoryIfMissing True (takeDirectory path) >>
      C8.writeFile path (serialize attrs)

    attrs :: Attrs
    attrs = Attrs
      { aNixShellBitUrl    = Just (nixShellBitUrl config)
      , aNixShellBitBranch = nixShellBitBranch config
      }

    serialize :: Attrs -> C8.ByteString
    serialize =
      C8.pack . show . unAnnotate . prettyExpr . embed inject
