import Data.Char (digitToInt)
import Data.List (elemIndex, unsnoc)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import Utils.InputProcessing (readInputLines)

type Solver = ProcessedInput -> Int

day :: String
day = "03"

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

type ProcessedInput = [[Int]]

procesInput :: [String] -> ProcessedInput
procesInput = map (map digitToInt)

runInput :: FilePath -> Solver -> IO ()
runInput file part = do
  raw <- readInputLines file
  let input = procesInput raw
  print $ part input

maxJoltage1 :: [Int] -> Int
maxJoltage1 bank = 10 * maxLeft + maxRight
  where
    (bank', _) = fromMaybe (error "empty bank") $ unsnoc bank

    maxLeft = maximum bank'
    maxLeftIdx = fromMaybe undefined $ elemIndex maxLeft bank'
    maxRight = maximum $ drop (maxLeftIdx + 1) bank

part1 :: Solver
part1 = sum . map maxJoltage1

part2 :: Solver
part2 input = length input
