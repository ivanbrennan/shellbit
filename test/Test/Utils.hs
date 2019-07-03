{-# LANGUAGE OverloadedStrings #-}

module Test.Utils
  ( capture
  , captureStderr
  , captureStdout
  , colorStrip
  , shouldMatch
  , silence
  , string
  , withInput
  ) where

import Control.Concurrent (newEmptyMVar, putMVar, readMVar)
import Control.Monad      (unless)
import Data.ByteString    (ByteString)
import Test.Hspec         (Expectation, HasCallStack, expectationFailure)
import Test.Main          (ProcessResult, captureProcessResult, prStderr,
                           prStdout, withStdin)
import Text.Regex.TDFA    ((=~))

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as E


captureStdout :: IO a -> IO ByteString
captureStdout =
  fmap prStdout . processResult


captureStderr :: IO a -> IO ByteString
captureStderr =
  fmap prStderr . processResult


processResult :: IO a -> IO ProcessResult
processResult =
  fmap fst . capture


silence :: IO a -> IO a
silence =
  fmap snd . capture


capture :: IO a -> IO (ProcessResult, a)
capture action =
  do
    var <- newEmptyMVar
    res <- captureProcessResult (action >>= putMVar var)
    val <- readMVar var
    pure (res, val)


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
