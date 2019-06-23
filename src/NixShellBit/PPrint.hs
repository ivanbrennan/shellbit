module NixShellBit.PPrint
  ( askSave
  , askYesNo
  , askUrl
  , fatalError
  , listItems
  , oopsNoProject
  , oopsNoVersion
  ) where

import Data.Text            (Text)
import System.Exit          (exitFailure)
import System.IO            (hFlush, stderr)
import System.Process.Typed (byteStringInput, proc, runProcess_, setStdin)
import Text.PrettyPrint.ANSI.Leijen (Doc, bold, brackets, char, colon,
                             debold, displayS, hcat, hPutDoc, hsep, line,
                             red, renderPretty, sep, space, text, vcat,
                             yellow, (<+>))

import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Text.IO as TIO


askSave :: FilePath -> IO Text
askSave path =
  ask $ vcat
      [ yellow (text "Config can be saved to") <+> text path
      , text "Save config?" <+> yesNo <> space
      ]


askYesNo :: IO Text
askYesNo =
  ask $ hsep
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


askUrl :: IO Text
askUrl =
  ask $ vcat
      [ yellow (text "NIX_SHELL_BIT_URL not found in environment or config.")
      , text "It should identify a git repo where we can find nix derivations"
      , text "for your project shells. E.g. git@github.com:Foo/nix-shells.git"
      , text "Please enter" <+> bold (text "NIX_SHELL_BIT_URL") <> text ": "
      ]


oopsNoProject :: IO a
oopsNoProject =
  die $ vcat
      [ yellow (text "Could not detect project")
      , text "Try --project=PROJECT"
      , line
      ]


oopsNoVersion :: IO a
oopsNoVersion =
  die $ vcat
      [ yellow (text "Could not detect version")
      , text "Try --version=VERSION"
      , line
      ]


fatalError :: String -> String -> IO a
fatalError cmd err =
  die $ hsep
      [ red (text "nix-shell-bit") <> colon
      , text cmd <> colon
      , text err
      ]


listItems :: String -> [String] -> IO ()
listItems hiItem items =
    runProcess_ $ setStdin (byteStringInput bs) (proc "column" [])
  where
    bs :: C8.ByteString
    bs = C8.pack $ displayS (renderPretty 1.0 80 doc) ""

    doc :: Doc
    doc = sep (map toDoc items)

    toDoc :: String -> Doc
    toDoc s | s == hiItem = bold (text s)
            | otherwise = debold (text s)


ask :: Doc -> IO Text
ask doc =
  hPutDoc stderr doc >>
  hFlush stderr >>
  TIO.getLine


die :: Doc -> IO a
die doc =
  hPutDoc stderr doc >>
  exitFailure
