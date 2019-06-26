{-# LANGUAGE OverloadedStrings #-}

module NixShellBit.PPrint
  ( askSave
  , askUrl
  , fatalError
  , listItems
  , oopsNoProject
  , oopsNoVersion
  , oopsNoVersions
  , oopsVersionUnavailable
  ) where

import Data.Char            (toLower)
import NixShellBit.Line     (readline)
import System.Exit          (exitFailure)
import System.IO            (Handle, hFlush, stderr, stdout)
import System.Process.Typed (byteStringInput, proc, readProcessStdout_, setStdin)
import Text.PrettyPrint.ANSI.Leijen (Doc, bold, brackets, char, colon, debold,
                             displayS, dquotes, hcat, hPutDoc, hsep, line, red,
                             renderPretty, sep, space, text, vcat, yellow, (<+>))

import qualified Data.ByteString.Lazy.Char8 as C8


askUrl :: IO String
askUrl =
  ask $ vcat
      [ yellow "NIX_SHELL_BIT_URL not found in environment or config."
      , "It should identify a git repo where we can find nix derivations"
      , "for your project shells. E.g. git@github.com:Foo/nix-shells.git"
      , "Please enter" <+> bold "NIX_SHELL_BIT_URL" <> colon <> space
      ]


askSave :: FilePath -> IO Bool
askSave path =
  do
    b <- readReply =<< askUser
    put line
    pure b
  where
    askUser :: IO String
    askUser =
        ask $ vcat
            [ yellow "Config can be saved to" <+> text path
            , "Save config?" <+> yesNo <> space
            ]

    askYesNo :: IO String
    askYesNo =
      ask' $ "Please answer y or n" <+> yesNo <> space

    yesNo :: Doc
    yesNo =
      brackets $ hcat
               [ bold (char 'Y')
               , char '/'
               , char 'n'
               ]

    readReply :: String -> IO Bool
    readReply reply =
      case map toLower reply of
        x | x `elem` ["yes", "y", ""] -> pure True
          | x `elem` ["no", "n"]      -> pure False
          | otherwise                 -> askYesNo >>= readReply


listItems :: String -> [String] -> IO ()
listItems = listItems' stdout


listItems' :: Handle -> String -> [String] -> IO ()
listItems' hdl focusItem items =
    columns >>= C8.hPut hdl >> hFlush hdl
  where
    columns :: IO C8.ByteString
    columns = readProcessStdout_ $
      setStdin (byteStringInput bs) (proc "column" [])

    bs :: C8.ByteString
    bs = C8.pack $ displayS (renderPretty 1.0 80 doc) ""

    doc :: Doc
    doc = sep (map toDoc items)

    toDoc :: String -> Doc
    toDoc x | x == focusItem = bold (text x)
            | otherwise = debold (text x)


oopsNoProject :: IO a
oopsNoProject =
  die $ vcat
      [ yellow "Could not detect project"
      , "Try --project=PROJECT"
      ]


oopsNoVersion :: IO a
oopsNoVersion =
  die $ vcat
      [ yellow "Could not detect version"
      , "Try --version=VERSION"
      ]


oopsNoVersions :: String -> IO a
oopsNoVersions project =
  die $ (yellow . hsep)
      [ "No versions available for project"
      , (dquotes . text) project
      ]


oopsVersionUnavailable :: String -> [String] -> IO a
oopsVersionUnavailable v versions =
  do
    put $ vcat
        [ yellow ("Version" <+> text v <+> "not found")
        , "The following versions are available by --version=VERSION"
        , mempty
        ]
    listItems' stderr v versions
    exitFailure


fatalError :: String -> String -> IO a
fatalError cmd err =
  die $ hsep
      [ red "nix-shell-bit" <> colon
      , red (text cmd) <> colon
      , text err
      ]


ask :: Doc -> IO String
ask doc =
  do
    put line
    ask' doc


ask' :: Doc -> IO String
ask' doc =
  do
    put doc
    readline


die :: Doc -> IO a
die doc =
  do
    put doc
    put line
    exitFailure


put :: Doc -> IO ()
put doc =
  do
    hPutDoc stderr doc
    hFlush stderr
