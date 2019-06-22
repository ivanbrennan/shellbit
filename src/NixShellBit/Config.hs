{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module NixShellBit.Config
  ( Config(..)
  , askConfig
  , readConfig
  ) where

import Control.Applicative       ((<|>))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Data.Text                 (Text)
import Dhall                     (Generic, Interpret, auto, inputFile)
import NixShellBit.PPrint        (askUrl)
import System.Directory          (findFile)
import System.Environment        (lookupEnv)
import System.FilePath           ((</>), (<.>))


newtype Config = Config
  { nixShellBitUrl :: Text
  } deriving (Generic, Show)

instance Interpret Config


readConfig :: IO (Maybe Config)
readConfig =
    runMaybeT (findConfig >>= parse)
  where
    findConfig :: MaybeT IO FilePath
    findConfig =
      do
        dir <- xdgConfig <|> homeConfig
        MaybeT (findFile [dir </> pkgName] (pkgName <.> ext))

    xdgConfig :: MaybeT IO FilePath
    xdgConfig =
      MaybeT (lookupEnv "XDG_CONFIG_HOME")

    homeConfig :: MaybeT IO FilePath
    homeConfig =
      MaybeT (lookupEnv "HOME") >>=
        MaybeT . pure . Just . (</> ".config")

    pkgName :: String
    pkgName = "nix-shell-bit"

    ext :: String
    ext = "dhall"

    parse :: FilePath -> MaybeT IO Config
    parse =
      MaybeT . fmap Just . inputFile auto


askConfig :: IO Config
askConfig = Config <$> askUrl
