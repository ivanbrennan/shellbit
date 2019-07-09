{-# LANGUAGE OverloadedStrings #-}

module NixShellBit.PPrint
  ( Doc
  , askSave
  , askUrl
  , fatal
  , line
  , listItems
  , noProject
  , noVersion
  , noVersions
  , putDocLn
  , putVersionNotFound
  ) where

import Data.Char            (toLower)
import NixShellBit.Line     (readline)
import System.IO            (Handle, hFlush, stderr, stdout)
import System.Process.Typed (byteStringInput, proc, readProcessStdout_, setStdin)
import Text.PrettyPrint.ANSI.Leijen (Doc, bold, brackets, char, colon, debold,
                             displayS, dquotes, hcat, hPutDoc, hsep, line, red,
                             renderPretty, sep, space, text, vcat, yellow, (<+>))

import qualified Data.ByteString.Lazy.Char8 as C


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


listItems :: [String] -> Maybe String -> IO ()
listItems = listItems' stdout


listItems' :: Handle -> [String] -> Maybe String -> IO ()
listItems' hdl items focusItem =
    columns >>= C.hPut hdl >> hFlush hdl
  where
    columns :: IO C.ByteString
    columns = readProcessStdout_ $
      setStdin (byteStringInput bs) (proc "column" [])

    bs :: C.ByteString
    bs = C.pack $ displayS (renderPretty 1.0 80 doc) ""

    doc :: Doc
    doc = sep (map toDoc items)

    toDoc :: String -> Doc
    toDoc x | Just x == focusItem = bold (text x)
            | otherwise = debold (text x)


noProject :: Doc
noProject =
  vcat [ yellow "Could not detect project"
       , "Try --project=PROJECT"
       ]


noVersion :: Doc
noVersion =
  vcat [ yellow "Could not detect version"
       , "Try --version=VERSION"
       ]


noVersions :: String -> Doc
noVersions project =
  (yellow . hsep)
    [ "No versions available for project"
    , dquotes (text project)
    ]


putVersionNotFound :: [String] -> String -> IO ()
putVersionNotFound versions version =
  do
    put $ vcat
        [ yellow ("Version" <+> text version <+> "not found")
        , "The following versions are available by --version=VERSION"
        , mempty
        ]
    listItems' stderr versions Nothing


fatal :: String -> String -> Doc
fatal cmd err =
  hsep [ red (text cmd) <> colon
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


putDocLn :: Doc -> IO ()
putDocLn doc =
  do
    put doc
    put line


put :: Doc -> IO ()
put doc =
  do
    hPutDoc stderr doc
    hFlush stderr
