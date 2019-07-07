{-# LANGUAGE OverloadedStrings #-}

module Test.Utils
  ( capture
  , captureExitCode
  , captureStderr
  , captureStdout
  , colorStrip
  , maybeCapture
  , prExitCode
  , prStderr
  , prStdout
  , shouldMatch
  , silence
  , string
  , withEnv
  , withInput
  ) where

import Control.Monad      (unless)
import Data.ByteString    (ByteString)
import GHC.Conc           (atomically, newTVar, readTVarIO, writeTVar)
import System.Exit        (ExitCode)
import Test.Hspec         (Expectation, HasCallStack, expectationFailure)
import Test.Main          (ProcessResult, captureProcessResult, prExitCode,
                           prStderr, prStdout, withEnv, withStdin)
import Text.Regex.TDFA    ((=~))
import UnliftIO.Exception (throwString)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as E


captureExitCode :: IO a -> IO ExitCode
captureExitCode =
  fmap prExitCode . processResult


captureStdout :: IO a -> IO ByteString
captureStdout =
  fmap prStdout . processResult


captureStderr :: IO a -> IO ByteString
captureStderr =
  fmap prStderr . processResult


processResult :: IO a -> IO ProcessResult
processResult =
  fmap fst . maybeCapture


silence :: IO a -> IO a
silence =
  fmap snd . capture


capture :: IO a -> IO (ProcessResult, a)
capture action =
  do
    (res, mval) <- maybeCapture action

    case mval of
      Nothing  -> throwString "action failed"
      Just val -> pure (res, val)


maybeCapture :: IO a -> IO (ProcessResult, Maybe a)
maybeCapture action =
  do
    var  <- atomically $ newTVar Nothing
    res  <- captureProcessResult
          $ action >>= atomically . writeTVar var . Just
    mval <- readTVarIO var

    pure (res, mval)


withInput :: [String] -> IO a -> IO a
withInput input =
  withStdin (C.unlines (map C.pack input))


string :: ByteString -> String
string = T.unpack . E.decodeUtf8 . colorStrip


colorStrip :: ByteString -> ByteString
colorStrip = go ""
  where
    go acc "" = acc
    go acc xs = go (acc <> a) (dropSGR b)
      where
        -- SGR sequences begin with `ESC[` and end with `m`
        (a, b) = B.breakSubstring "\ESC[" xs
        dropSGR = B.drop 1 . B.dropWhile (/= 0x6d)


infix 1 `shouldMatch`

shouldMatch
  :: HasCallStack
  => String
  -> String
  -> Expectation
shouldMatch result expected =
  unless (result =~ expected) $
    expectationFailure $
      show result ++ "does not match" ++ show expected
