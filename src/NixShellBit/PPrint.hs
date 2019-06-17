module NixShellBit.PPrint
  ( Doc
  , line
  , die
  , string
  , text
  , vcat
  , hPutDoc
  , yellow
  , red
  ) where

import System.Exit (exitFailure)
import System.IO   (stderr)
import Text.PrettyPrint.ANSI.Leijen (Doc, string, text, vcat,
                                     hPutDoc, yellow, red, line)


die :: Doc -> IO a
die doc =
    hPutDoc stderr doc >> exitFailure
