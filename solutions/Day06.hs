import Data.Bifunctor (Bifunctor (first))
import Data.Matrix (Matrix (nrows), getRow, transpose)
import Data.Maybe (fromMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.Environment (getArgs)
import qualified Utils.Grid.Matrix as G
import Utils.InputProcessing (parseInt, readInputLines)

type Solver = ProcessedInput -> Int

day :: String
day = "06"

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

type ProcessedInput = G.Grid String

procesInput :: [String] -> ProcessedInput
procesInput = transpose . G.gridify . map words

runInput :: FilePath -> Solver -> IO ()
runInput file part = do
  raw <- readInputLines file
  let input = procesInput raw
  print $ part input

operate :: String -> Vector Int -> Int
operate "*" = V.product
operate "+" = V.sum
operate _ = error "unexpected operator"

part1 :: Solver
part1 m = res
  where
    res = foldr checkRow 0 [1 .. nrows m]

    checkRow i acc =
      let row = getRow i m
          (vecNums, op) =
            first (V.map parseInt) $ fromMaybe undefined $ V.unsnoc row
       in acc + operate op vecNums

part2 :: Solver
part2 m = length m
