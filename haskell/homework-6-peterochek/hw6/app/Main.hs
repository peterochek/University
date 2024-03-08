{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  ) where

import System.Process (system)

import System.Console.GetOpt
import System.Environment (getArgs)

import Control.Concurrent (threadDelay)
import Data.Grid (unGrid)
import Data.List (intercalate)
import Data.ListZipper (truncateLZ)
import HW6.T3 (Comonad19Grid, configure, simulate)

data Options =
  Options
    { optProb       :: Double
    , optIncub      :: Int
    , optIll        :: Int
    , optImmun      :: Int
    , optGridSize   :: Int
    , optIterations :: Int
    , optSeed       :: Int
    }

defaultOptions :: Options
defaultOptions =
  Options
    { optProb = 0.4
    , optIncub = 2
    , optIll = 5
    , optImmun = 7
    , optGridSize = 10
    , optIterations = 20
    , optSeed = 42
    }

options :: [OptDescr (Options -> Options)]
options =
  [ Option
      []
      ["prob"]
      (ReqArg (\p opts -> opts {optProb = read p}) "PROBABILITY")
      "Infection probability"
  , Option
      []
      ["incub"]
      (ReqArg (\i opts -> opts {optIncub = read i}) "INCUBATION")
      "Incubation period duration"
  , Option
      []
      ["ill"]
      (ReqArg (\i opts -> opts {optIll = read i}) "ILLNESS")
      "Illness duration"
  , Option
      []
      ["immun"]
      (ReqArg (\i opts -> opts {optImmun = read i}) "IMMUNITY")
      "Immunity duration"
  , Option
      []
      ["grid-size"]
      (ReqArg (\s opts -> opts {optGridSize = read s}) "SIZE")
      "Output grid size"
  , Option
      []
      ["iterations"]
      (ReqArg (\n opts -> opts {optIterations = read n}) "ITERATIONS")
      "Number of simulation iterations"
  , Option
      []
      ["seed"]
      (ReqArg (\s opts -> opts {optSeed = read s}) "SEED")
      "Initial random seed (default 42)"
  ]

parseArgs :: IO Options
parseArgs = do
  args <- getArgs
  case getOpt Permute options args of
    (o, _, []) -> return $ foldl (flip id) defaultOptions o
    (_, _, errs) ->
      ioError (userError (concat errs ++ usageInfo header options))
  where
    header = "Usage: comonad19 [OPTION...]"

data DisplayGrid =
  DisplayGrid
    { dgGrid :: Comonad19Grid
    , dgSize :: Int
    }

instance Show DisplayGrid where
  show dg = intercalate "\n" gridRepr
    where
      inner = unGrid (dgGrid dg)
      size = dgSize dg
      strip = fmap (truncateLZ size) inner
      truncated = truncateLZ size strip
      gridRepr = map (concatMap show) truncated

output :: [DisplayGrid] -> IO ()
output steps = do
  mapM_ (\s -> flush >> print s >> threadDelay 100000) steps
  where
    flush = do
      _ <- system "clear"
      return ()

main :: IO ()
main = do
  Options {..} <- parseArgs
  case configure optProb optIncub optIll optImmun of
    Just config -> do
      let grids = take optIterations $ simulate config optSeed
      let displayGrids = map (`DisplayGrid` optGridSize) grids
      output displayGrids
    Nothing -> putStrLn "Ensure params are correct!"
