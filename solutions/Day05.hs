import Data.Bifunctor (Bifunctor (bimap))
import System.Environment (getArgs)
import Utils.InputProcessing (
  breakLine,
  parseInt,
  readInputLines,
 )

type Solver = ProcessedInput -> Int

day :: String
day = "05"

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

type ProcessedInput = ([(Int, Int)], [Int])

procesInput :: [String] -> ProcessedInput
procesInput strs = (ranges, ids)
  where
    ranges' = takeWhile (not . null) strs
    ranges = map (bimap parseInt (parseInt . drop 1) . breakLine '-') ranges'

    ids' = drop 1 $ dropWhile (not . null) strs
    ids = map parseInt ids'

runInput :: FilePath -> Solver -> IO ()
runInput file part = do
  raw <- readInputLines file
  let input = procesInput raw
  print $ part input

isBetween :: Int -> (Int, Int) -> Bool
isBetween x (i, j) = i <= x && x <= j

part1 :: Solver
part1 (ranges, ids) = length $ filter id areFresh
  where
    areFresh = map (\i -> any (i `isBetween`) ranges) ids

part2 :: Solver
part2 input = length input
