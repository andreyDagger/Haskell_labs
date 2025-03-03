module HW6.T3
  ( Config (..)
  , Cell (..)
  , CellState (..)
  , Comonad19Grid
  , gridToString
  , simulate
  ) where

import           System.Random   (StdGen, mkStdGen, random)

import           Control.Comonad
import           Control.Monad   (liftM2)
import           Data.Grid       (Grid (..), gDown, gLeft, gRight, gUp, gWrite)
import           Data.List       (intercalate)
import           Data.ListZipper

data Config = Config
  { probability      :: Double
  , incubationPeriod :: Int
  , illnessDuration  :: Int
  , immunityDuration :: Int
  , seed             :: Int
  } deriving Show

data CellState
  = Healthy
  | Infected Int
  | Ill Int
  | Immune Int
  deriving Show

data Cell = Cell
  { cellState :: CellState
  , cellRand  :: StdGen
  }

type Comonad19Grid = Grid Cell

cellToString :: Cell -> String
cellToString (Cell (Infected _) _) = "i"
cellToString (Cell (Immune _) _)   = "@"
cellToString (Cell (Ill _) _)      = "#"
cellToString (Cell Healthy _)      = "_"

-- | Creates an infinite list of grids using the given configuration.
-- Each element of this list represents one infection simulation step.
--
-- This function may take additional parameters (e.g. initial seed for random).
simulate :: Config -> [Comonad19Grid]
simulate config = genList (genGrid config) (evolve config)

genList :: a -> (a -> a) -> [a]
genList a f = a : genList (f a) f

neighbors :: [Grid a -> Grid a]
neighbors = horizontals ++ verticals ++ liftM2 (.) horizontals verticals
  where horizontals = [gLeft, gRight]
        verticals = [gUp, gDown]

fullNeighbors :: Comonad19Grid -> [Cell]
fullNeighbors g = map dNeighbors neighbors
  where dNeighbors :: (Grid Cell -> Grid Cell) -> Cell
        dNeighbors d = extract (d g)

processInfected :: Config -> Int -> StdGen -> Cell
processInfected cfg 0 gen  = Cell (Ill (illnessDuration cfg)) gen
processInfected _ time gen = Cell (Infected (time - 1)) gen

processImmune :: Config -> Int -> StdGen -> Cell
processImmune _ 0 gen    = Cell Healthy gen
processImmune _ time gen = Cell (Immune (time - 1)) gen

processIll :: Config -> Int -> StdGen -> Cell
processIll cfg 0 gen  = Cell (Immune (immunityDuration cfg)) gen
processIll _ time gen = Cell (Ill (time - 1)) gen

canInfect :: Cell -> Int
canInfect (Cell (Infected _) _) = 1
canInfect (Cell (Immune _) _)   = 0
canInfect (Cell (Ill _) _)      = 1
canInfect (Cell Healthy _)      = 0

processHealthy :: Config -> StdGen -> [Cell] -> Cell
processHealthy cfg gen around =
  let infecters = sum (map canInfect around)
      probNotInfectedOnce = 1.0 - probability cfg
      probabilityNotInfected = 1.0 - probNotInfectedOnce ^ infecters
      (val, newGen) = random gen
  in if val <= probabilityNotInfected then
    Cell (Infected (incubationPeriod cfg)) newGen
  else
    Cell Healthy newGen

rule :: Config -> Comonad19Grid -> Cell
rule cfg g = case ((cellState $ extract g, cellRand $ extract g), fullNeighbors g) of
  ((Ill time, gen), _)      -> processIll cfg time gen
  ((Infected time, gen), _) -> processInfected cfg time gen
  ((Immune time, gen), _)   -> processImmune cfg time gen
  ((Healthy, gen), around)  -> processHealthy cfg gen around

evolve :: Config -> Comonad19Grid -> Comonad19Grid
evolve c = extend (rule c)

genGrid :: Config -> Comonad19Grid
genGrid cfg = let seedsGrid = Grid $ duplicate $ LZ (iterate (fst . random . mkStdGen) 228) 0 (iterate (fst . random . mkStdGen) 1488)
  in gWrite (Cell (Infected (incubationPeriod cfg)) (mkStdGen (seed cfg))) (Cell Healthy . mkStdGen <$> seedsGrid)

gridToString :: Comonad19Grid -> Int -> String
gridToString g size = let (Grid strGrid) = cellToString <$> g
  in intercalate "\n" (map (intercalate "" . flip toList size) (toList strGrid size))
