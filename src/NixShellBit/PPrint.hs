{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module NixShellBit.PPrint
  ( askSave
  , askUrl
  , askYesNo
  , fatalError
  , listItems
  , oopsNoProject
  , oopsNoVersion
  , oopsNoVersions
  , oopsVersionUnavailable
  ) where

import Data.Text            (Text, pack, toLower)
import NixShellBit.Line     (readline)
import System.Exit          (exitFailure)
import System.IO            (Handle, hFlush, stderr, stdout)
import System.Process.Typed (byteStringInput, proc, readProcessStdout_, setStdin)
import Text.PrettyPrint.ANSI.Leijen (Doc, bold, brackets, char, colon, debold,
                             displayS, dquotes, hcat, hPutDoc, hsep, line, red,
                             renderPretty, sep, space, text, vcat, yellow, (<+>))

import qualified Data.ByteString.Lazy.Char8 as C8


askUrl :: IO Text
askUrl =
  ask $ vcat
      [ yellow (text "NIX_SHELL_BIT_URL not found in environment or config.")
      , text "It should identify a git repo where we can find nix derivations"
      , text "for your project shells. E.g. git@github.com:Foo/nix-shells.git"
      , text "Please enter" <+> bold (text "NIX_SHELL_BIT_URL") <> text ": "
      ]


askSave :: FilePath -> IO Bool
askSave path =
  do
    b <- readReply =<< askUser
    put line
    pure b
  where
    askUser :: IO Text
    askUser =
        ask $ vcat
            [ yellow (text "Config can be saved to") <+> text path
            , text "Save config?" <+> yesNo <> space
            ]

    readReply :: Text -> IO Bool
    readReply reply =
      case toLower reply of
        x | x `elem` ["yes", "y", ""] -> pure True
          | x `elem` ["no", "n"]      -> pure False
          | otherwise                 -> askYesNo >>= readReply


askYesNo :: IO Text
askYesNo =
  ask' $ hsep
       [ text "Please answer y or n"
       , yesNo <> space
       ]


yesNo :: Doc
yesNo =
  brackets $ hcat
           [ bold (char 'Y')
           , char '/'
           , char 'n'
           ]


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
    toDoc s | s == focusItem = bold (text s)
            | otherwise = debold (text s)


oopsNoProject :: IO a
oopsNoProject =
  die $ vcat
      [ yellow (text "Could not detect project")
      , text "Try --project=PROJECT"
      ]


oopsNoVersion :: IO a
oopsNoVersion =
  die $ vcat
      [ yellow (text "Could not detect version")
      , text "Try --version=VERSION"
      ]


oopsNoVersions :: String -> IO a
oopsNoVersions name =
  die $ (yellow . hsep)
      [ text "No versions available for project"
      , dquotes (text name)
      ]


oopsVersionUnavailable :: String -> [String] -> IO a
oopsVersionUnavailable v versions =
  do
    put $ vcat
        [ yellow (text "Version" <+> text v <+> "not found")
        , text "The following versions are available by --version=VERSION"
        , mempty
        ]
    listItems' stderr v versions
    exitFailure


fatalError :: String -> String -> IO a
fatalError cmd err =
  die $ hsep
      [ red (text "nix-shell-bit") <> colon
      , red (text cmd) <> colon
      , text err
      ]


ask :: Doc -> IO Text
ask doc =
  do
    put line
    ask' doc


ask' :: Doc -> IO Text
ask' doc =
  do
    put doc
    pack <$> readline


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
