module Main (main) where
import           HW6.T3              (Comonad19Grid, Config (..), gridToString,
                                      simulate)
import           Options.Applicative

printGrids :: Int -> [Comonad19Grid] -> IO ()
printGrids _ [] = return ()
printGrids sz (x:xs) = do
    putStrLn (gridToString x sz)
    printGrids sz xs

data LocalConfig = CFG
  {
    prob       :: Double,
    incub      :: Int,
    ill        :: Int,
    immun      :: Int,
    gridSize   :: Int,
    iterations :: Int,
    sed        :: Int
  } deriving Show

configParser :: Parser LocalConfig
configParser = CFG
  <$> option auto
      ( long "prob"
      <> short 'p'
      <> value 0.5
      <> showDefault
      <> help "Infection probability"
      )
  <*> option auto
      ( long "incub"
      <> short 'i'
      <> value 2
      <> showDefault
      <> help "Incubation period duration"
      )
  <*> option auto
      ( long "ill"
      <> short 's'
      <> value 2
      <> showDefault
      <> help "Illness duration"
      )
  <*> option auto
      ( long "immun"
      <> short 'h'
      <> value 2
      <> showDefault
      <> help "Immunity duration"
      )
  <*> option auto
      ( long "grid-size"
      <> short 'g'
      <> value 5
      <> showDefault
      <> help "Output grid size. Note that `Grid` type represents an infinite grid."
      )
  <*> option auto
      ( long "iterations"
      <> short 'm'
      <> value 10
      <> showDefault
      <> help "The number of simulation iterations"
      )
  <*> option auto
      ( long "seed"
      <> short 'r'
      <> value 420
      <> showDefault
      <> help "Random generator seed"
      )

main :: IO ()
main = process =<< execParser opts
  where
    opts = info (configParser <**> helper)
      ( fullDesc
      <> progDesc "Simulating Covid-19")

process :: LocalConfig -> IO ()
process c = do
    let cfg = Config {
        probability = prob c,
        incubationPeriod = incub c,
        illnessDuration = ill c,
        immunityDuration = immun c,
        seed = sed c
    }
    let grids = take (iterations c) (simulate cfg)
    printGrids (gridSize c) grids
