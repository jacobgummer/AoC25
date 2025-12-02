import Data.Bifunctor (Bifunctor (second))
import Data.List.Split (splitOn)
import System.Environment (getArgs)
import Utils.InputProcessing (
  breakLine,
  parseInt,
  readInputLines,
 )

type Solver = ProcessedInput -> Int

day :: String
day = "02"

inputFile, testFile :: FilePath
inputFile = "data/inputs/day" ++ day ++ ".txt"
testFile  = "data/tests/day" ++ day ++ ".txt"

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
