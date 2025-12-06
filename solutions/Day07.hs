import Data.Heap (Heap)
import qualified Data.Heap as H
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map (Map)
import qualified Data.Map as M
import Data.Matrix hiding ((!))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.Environment (getArgs)
import Utils.Grid.Matrix (CharGrid, Grid, GridPos, (!))
import qualified Utils.Grid.Matrix as G
import Utils.InputProcessing

type Solver = ProcessedInput -> Int

day :: String
day = "07"

inputFile, testFile :: FilePath
inputFile = "data/inputs/day" ++ day ++ ".txt"
testFile = "data/tests/day" ++ day ++ ".txt"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["p1"] -> runInput inputFile part1
    ["p2"] -> runInput inputFile part2
    ["p1", "-t"] -> runInput testFile part1
    ["p2", "-t"] -> runInput testFile part2
    _ -> putStrLn $ "Usage: ./day <" ++ day ++ "> <p1|p2> [-t]"

-- TODO: Adjust this.
type ProcessedInput = [[Int]]

-- procesInputT :: [Text] -> ProcessedInput
-- procesInputT txts = undefined

procesInput :: [String] -> ProcessedInput
procesInput strs = undefined

runInput :: FilePath -> Solver -> IO ()
runInput file part = do
  raw <- readInputLines file
  let input = procesInput raw
  print $ part input

part1 :: Solver
part1 input = length input

part2 :: Solver
part2 input = length input
