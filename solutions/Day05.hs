import Data.Bifunctor (Bifunctor (bimap))
import Data.List (sort)
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
    both f = bimap f f

    ranges' = takeWhile (not . null) strs
    ranges = map (both parseInt . breakLine '-') ranges'

    ids' = drop 1 $ dropWhile (not . null) strs
    ids = map parseInt ids'

runInput :: FilePath -> Solver -> IO ()
runInput file part = do
  raw <- readInputLines file
  let input = procesInput raw
  print $ part input

part1 :: Solver
part1 (ranges, ids) = length $ filter id areFresh
  where
    isBetween x (i, j) = i <= x && x <= j

    areFresh = map (\i -> any (i `isBetween`) ranges) ids

part2 :: Solver
part2 = sum . map rangeSpan . findRanges . sort . fst
  where
    rangeSpan (i, j) = j - i + 1

    overlaps (s1, e1) (s2, e2) = s1 <= e2 && s2 <= e1

    findRanges = foldl update []

    update [] r = [r]
    update crs@(r'@(s', e') : crs') r@(_, e)
      | overlaps r r' = (s', max e e') : crs'
      | otherwise = r : crs
