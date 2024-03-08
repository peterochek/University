module HW5.Utils
  ( isBetween
  , isInt
  , isInteger
  , makeAbsolute
  ) where

import Data.Ratio (denominator)
import System.Directory (canonicalizePath, getCurrentDirectory)
import System.FilePath ((</>))

isBetween :: (Ord a) => a -> a -> a -> Bool
isBetween l r x = l <= x && x <= r

isInteger :: Rational -> Bool
isInteger x = denominator x == 1

isInt :: Rational -> Bool
isInt x = isInteger x && isBetween lower upper x
  where
    lower = toRational (minBound :: Int)
    upper = toRational (maxBound :: Int)

makeAbsolute :: FilePath -> IO FilePath
makeAbsolute path = do
  curDir <- getCurrentDirectory
  canonicalizePath (curDir </> path)
