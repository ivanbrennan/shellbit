{-# LANGUAGE LambdaCase #-}

module Shellbit.Line
  ( readline
  ) where

import System.Console.Haskeline (defaultSettings, getInputLine, noCompletion,
                                 runInputT, setComplete)
import System.Exit              (exitFailure)
import System.IO.Error          (eofErrorType, isEOFError, mkIOError)
import UnliftIO.Exception       (catchIO, throwIO)


readline :: IO String
readline =
    readInput `catchIO` handleEOF
  where
    readInput :: IO String
    readInput = runInputT (setComplete noCompletion defaultSettings) $
      getInputLine "" >>= \case
        Nothing    -> throwIO (mkIOError eofErrorType "" Nothing Nothing)
        Just reply -> pure reply

    handleEOF :: IOError -> IO String
    handleEOF e
      | isEOFError e = exitFailure
      | otherwise    = throwIO e
