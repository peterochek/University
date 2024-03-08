module HW6.T3
  ( Config(..)
  , Cell(..)
  , CellState(..)
  , Comonad19Grid
  , simulate
  , configure
  ) where

import System.Random (StdGen, mkStdGen, random)

import Data.Grid (Grid (..), insertGrid, shifts)

import Control.Comonad (Comonad (..))
import Data.ListZipper (shift)

type Comonad19Grid = Grid Cell

createGrid :: Config -> Int -> Comonad19Grid
createGrid config seed =
  let zeroCell = Cell Healthy (mkStdGen seed)
      zeroRow = shift leftRand rightRand zeroCell
      emptyGrid = Grid $ shift upRand downRand zeroRow
   in insertGrid
        (zeroCell {cellState = Infected $ incubationPeriod config})
        emptyGrid
  where
    newRand f p@(Cell _ randGen) = p {cellRand = newRandGen}
      where
        (num, _) = random randGen
        newRandGen = mkStdGen (f num)
    leftRand = newRand (+ 1)
    rightRand = newRand (+ 2)
    upRand = fmap (newRand (+ 3))
    downRand = fmap (newRand (+ 4))

data Config =
  Config
    { probability      :: Double
    , incubationPeriod :: Int
    , illnessDuration  :: Int
    , immunityDuration :: Int
    }
  deriving (Show)

verifyConfig :: Double -> Int -> Int -> Int -> Bool
verifyConfig probability' incubationPeriod' illnessDuration' immunityDuration' =
  probability' >= 0 &&
  probability' <= 1 &&
  incubationPeriod' > 0 && illnessDuration' > 0 && immunityDuration' > 0

configure :: Double -> Int -> Int -> Int -> Maybe Config
configure probability' incubationPeriod' illnessDuration' immunityDuration' =
  if verifyConfig
       probability'
       incubationPeriod'
       illnessDuration'
       immunityDuration'
    then Just $
         Config
           { probability = probability'
           , incubationPeriod = incubationPeriod'
           , illnessDuration = illnessDuration'
           , immunityDuration = immunityDuration'
           }
    else Nothing

data CellState
  = Healthy
  | Infected Int
  | Ill Int
  | Immune Int

instance Show CellState where
  show Healthy      = "_"
  show (Infected _) = "i"
  show (Ill _)      = "#"
  show (Immune _)   = "@"

data Cell =
  Cell
    { cellState :: CellState
    , cellRand  :: StdGen
    }

instance Show Cell where
  show = show . cellState

isCellDangerous :: Cell -> Bool
isCellDangerous = state . cellState
  where
    state (Infected _) = True
    state (Ill _)      = True
    state _            = False

updateCellState :: Config -> Comonad19Grid -> Cell -> Cell
updateCellState config grid cell =
  case cellState cell of
    Healthy    -> contact config grid
    Infected 1 -> cell {cellState = Ill $ illnessDuration config}
    Infected n -> cell {cellState = Infected (n - 1)}
    Ill 1      -> cell {cellState = Immune $ immunityDuration config}
    Ill n      -> cell {cellState = Ill (n - 1)}
    Immune 1   -> cell {cellState = Healthy}
    Immune n   -> cell {cellState = Immune (n - 1)}

isCellInDanger :: Comonad19Grid -> Bool
isCellInDanger grid =
  any (isCellDangerous . (\arrow -> extract $ arrow grid)) shifts

contact :: Config -> Comonad19Grid -> Cell
contact config grid =
  if isCellInDanger grid && genProb <= configProb
    then Cell (Ill $ incubationPeriod config) newCellRand
    else Cell Healthy newCellRand
  where
    configProb = probability config
    (genProb, newCellRand) = random . cellRand . extract $ grid

propagate :: Config -> Comonad19Grid -> Comonad19Grid
propagate config = extend updateCell
  where
    updateCell g = updateCellState config g (extract g)

simulate :: Config -> Int -> [Comonad19Grid]
simulate config seed = iterate (propagate config) initialGrid
  where
    initialGrid = createGrid config seed
