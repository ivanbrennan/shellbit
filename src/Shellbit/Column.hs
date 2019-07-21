module NixShellBit.Column
  ( grid
  , terminalWidth
  ) where

import Control.Applicative       ((<|>))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Data.Foldable             (foldl')
import Data.List                 (unfoldr)
import Data.Maybe                (fromMaybe)
import System.Environment        (lookupEnv)
import Text.Read                 (readMaybe)

import qualified System.Console.Terminal.Size as Terminal


type Column = (String, Padding)

type Padding = String


tabWidth :: Int
tabWidth = 8


grid :: [String] -> Int -> [[Column]]
grid [] _     = []
grid xs width =
    if maxLength >= width
      then map (\x -> [(x, "")]) items
      else map (unfoldr column) [0 .. numrows - 1]
  where
    maxLength :: Int
    maxLength =
      foldl' (\z -> max z . length) 0 items

    column :: Int -> Maybe (Column, Int)
    column i =
        if i >= numItems
          then Nothing
          else Just ((item, padding), i + numrows)
      where
        item :: String
        item = items !! i

        padding :: Padding
        padding = replicate padCount '\t'

        padCount :: Int
        padCount =
          if i + numrows >= numItems
            then 0
            else qPad + signum rPad

        qPad, rPad :: Int
        (qPad, rPad) = quotRem (columnWidth - length item) tabWidth

    numrows :: Int
    numrows = qRows + signum rRows

    qRows, rRows :: Int
    (qRows, rRows) = quotRem numItems numcols

    numcols :: Int
    numcols = max 1 (width `quot` columnWidth)

    columnWidth :: Int
    columnWidth = addTab maxLength

    addTab :: Int -> Int
    addTab pos =
      pos + tabWidth - (pos `rem` tabWidth)

    numItems :: Int
    numItems = length items

    items :: [String]
    items = filter (not . null) xs


terminalWidth :: IO Int
terminalWidth =
    fromMaybe defaultWidth <$> runMaybeT (fromTTY <|> fromEnv)
  where
    fromTTY :: MaybeT IO Int
    fromTTY = s >>= w
      where
        s = MaybeT Terminal.size
        w = MaybeT . pure . Just . Terminal.width

    fromEnv :: MaybeT IO Int
    fromEnv = c >>= i
      where
        c = MaybeT (lookupEnv "COLUMNS")
        i = MaybeT . pure . readMaybe

    defaultWidth :: Int
    defaultWidth = 80
