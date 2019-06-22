{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module NixShellBit.Config
  ( Config(..)
  , askConfig
  , findConfig
  , readConfig
  , saveConfig
  ) where

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
import System.FilePath           (takeDirectory, takeFileName, (</>), (<.>))

import qualified Data.ByteString.Char8 as C8


newtype Config = Config
  { nixShellBitUrl :: Text
  } deriving (Generic, Show)

instance Interpret Config
instance Inject Config


findConfig :: IO (Maybe FilePath)
findConfig =
  do
    path <- configPath
    findFile [takeDirectory path] (takeFileName path)


readConfig :: FilePath -> IO Config
readConfig =
  inputFile auto


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


askConfig :: IO Config
askConfig = Config <$> askUrl


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
