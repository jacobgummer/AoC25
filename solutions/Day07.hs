import Data.Matrix hiding ((!))
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Vector as V
import System.Environment (getArgs)
import Utils.Grid.Matrix (CharGrid, GridPos, (!))
import qualified Utils.Grid.Matrix as G
import Utils.InputProcessing (readInputLines)

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

type ProcessedInput = CharGrid

procesInput :: [String] -> ProcessedInput
procesInput = G.gridify

runInput :: FilePath -> Solver -> IO ()
runInput file part = do
  raw <- readInputLines file
  let input = procesInput raw
  print $ part input

findSplitters :: CharGrid -> GridPos -> Set GridPos
findSplitters grid pos =
  S.filter (\p -> grid ! p == '^') $ go (S.singleton pos) S.empty
  where
    go curr visited
      | S.null curr = visited
      | otherwise =
          let visited' = visited `S.union` curr
              next = S.fromList $ concatMap step $ S.toList curr
           in go next visited'

    step pos'@(i, j)
      | not $ G.inBounds grid pos' = []
      | grid ! pos' == '^' = [(i + 1, j - 1), (i + 1, j + 1)]
      | otherwise =
          let pos'' = (i + 1, j)
           in [(i + 1, j) | G.inBounds grid pos'']

part1 :: Solver
part1 grid = S.size $ findSplitters grid (0, start_j)
  where
    start_j = fromMaybe undefined $ V.findIndex (== 'S') $ getRow 1 grid

part2 :: Solver
part2 grid = length grid
