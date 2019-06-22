{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module NixShellBit.Config
  ( Config(..)
  , configPath
  , getConfig
  , saveConfig
  ) where

import Control.Applicative       ((<|>))
import Control.Monad             (when)
import Data.Text                 (Text, toLower)
import Data.Text.Prettyprint.Doc (unAnnotate)
import Dhall                     (Generic, Inject, Interpret,
                                  auto, embed, inject, inputFile)
import Dhall.Pretty              (prettyExpr)
import NixShellBit.PPrint        (askSave, askUrl, askYesNo)
import System.Directory          (XdgDirectory(XdgConfig),
                                  createDirectoryIfMissing,
                                  findFile, getXdgDirectory)
import System.Environment        (lookupEnv)
import System.FilePath           (takeDirectory, takeFileName, (</>), (<.>))

import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T


newtype Config = Config
  { nixShellBitUrl :: Text
  } deriving (Generic, Show)

instance Interpret Config
instance Inject Config


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
  do
    e <- fromEnv
    f <- fromFile
    maybe askConfig pure (e <|> f)
  where
    fromEnv :: IO (Maybe Config)
    fromEnv =
      do
        url <- lookupEnv "NIX_SHELL_BIT_URL"
        pure (Config . T.pack <$> url)

    fromFile :: IO (Maybe Config)
    fromFile =
      do
        path <- configPath
        file <- findFile [takeDirectory path] (takeFileName path)
        traverse (inputFile auto) file

    askConfig :: IO Config
    askConfig = Config <$> askUrl


saveConfig :: Config -> IO ()
saveConfig config =
  do
    path    <- configPath
    confirm <- readReply =<< askSave path

    when confirm (writeConfig path)
  where
    readReply :: Text -> IO Bool
    readReply reply =
      case toLower reply of
        x | x `elem` ["yes", "y", ""] -> pure True
          | x `elem` ["no", "n"]      -> pure False
          | otherwise                 -> askYesNo >>= readReply

    writeConfig :: FilePath -> IO ()
    writeConfig path =
      createDirectoryIfMissing True (takeDirectory path) >>
      C8.writeFile path (serialize config)

    serialize :: Config -> C8.ByteString
    serialize =
      C8.pack . show . unAnnotate . prettyExpr . embed inject
