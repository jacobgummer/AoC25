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

maxJoltage2 :: [Int] -> Int
maxJoltage2 bank = sum $ zipWith (\b p -> b * 10 ^ p) batteries powers
  where
    k = 12

    n = length bank
    initSkippedLeft = n - k + 1
    powers = reverse [0 .. k - 1] :: [Int]

    findBatteries [] _ acc = reverse acc
    findBatteries [b] _ acc = reverse $ b : acc
    findBatteries bank'@(b : bank'') skippedLeft acc
      | skippedLeft == 0 = findBatteries bank'' 0 (b : acc)
      | otherwise =
          let possibleBatteries = take skippedLeft bank'
              highest = maximum possibleBatteries
              idxHighest =
                fromMaybe (error "idxHighest") $
                  elemIndex highest possibleBatteries
              skippedLeft' = skippedLeft - idxHighest
           in findBatteries
                (drop (idxHighest + 1) bank')
                skippedLeft'
                (highest : acc)

    batteries = findBatteries bank initSkippedLeft []

part2 :: Solver
part2 = sum . map maxJoltage2
