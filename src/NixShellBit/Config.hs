{-# LANGUAGE OverloadedStrings #-}

module NixShellBit.Config
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
import NixShellBit.PPrint        (askSave, askUrl)
import System.Directory          (XdgDirectory(XdgConfig),
                                  createDirectoryIfMissing, doesFileExist,
                                  findFile, getXdgDirectory)
import System.Environment        (lookupEnv)
import System.FilePath           (takeDirectory, takeFileName, (</>), (<.>))

import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Dhall


data Config = Config
  { cNixShellBitUrl    :: Text
  , cNixShellBitBranch :: Maybe Text
  } deriving Show


data Attrs = Attrs
  { nixShellBitUrl    :: Option (Last Text)
  , nixShellBitBranch :: Option (Last Text)
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
    fromEnv :: IO Attrs
    fromEnv =
      Attrs <$> envOption "NIX_SHELL_BIT_URL"
            <*> envOption "NIX_SHELL_BIT_BRANCH"

    fromFile :: IO Attrs
    fromFile =
      do
        path <- configPath
        file <- findFile [takeDirectory path] (takeFileName path)
        maybe (pure mempty) load file

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
    package = "nix-shell-bit"

    fileName :: FilePath
    fileName = "config" <.> "dhall"


load :: FilePath -> IO Attrs
load =
    inputFile attrs
  where
    attrs :: Type Attrs
    attrs =
      record ( Attrs <$> field "nixShellBitUrl"    opt
                     <*> field "nixShellBitBranch" opt )

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
      C.writeFile path (serialize attrs)

    attrs :: Attrs
    attrs = Attrs
      { nixShellBitUrl    = (toOption . Just . cNixShellBitUrl) config
      , nixShellBitBranch = (toOption . cNixShellBitBranch) config
      }


serialize :: Attrs -> C.ByteString
serialize =
    C.pack . show . unAnnotate . prettyExpr . embed input
  where
    input :: InputType Attrs
    input =
      inputRecord ( adapt >$< inputField "nixShellBitUrl"
                          >*< inputField "nixShellBitBranch" )

    adapt :: Attrs -> (Maybe Text, Maybe Text)
    adapt attrs =
      ( toMaybe (nixShellBitUrl    attrs)
      , toMaybe (nixShellBitBranch attrs)
      )
