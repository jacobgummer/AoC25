import System.Environment (getArgs)
import Utils.InputProcessing

type Solver = ProcessedInput -> Int

day :: String
day = "01"

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
type ProcessedInput = [String]

-- procesInputT :: [Text] -> ProcessedInput
-- procesInputT txts = undefined

procesInput :: [String] -> ProcessedInput
procesInput = id

runInput :: FilePath -> Solver -> IO ()
runInput file part = do
  raw <- readInputLines file
  let input = procesInput raw
  print $ part input

part1 :: Solver
part1 = snd . foldl go (50, 0)
  where
    go (num, acc) ('L' : sNum) =
      let num' = (num - parseInt sNum) `mod` 100
       in if num' == 0
            then (num', succ acc)
            else (num', acc)
    go (num, acc) ('R' : sNum) =
      let num' = (num + parseInt sNum) `mod` 100
       in if num' == 0
            then (num', succ acc)
            else (num', acc)
    go _ _ = error "didn't expect to end here"

part2 :: Solver
part2 = snd . foldl go (50, 0)
  where
    go (num, acc) ('L' : sNum) =
      let k = parseInt sNum
          pZero = passedZeroL num k
          num' = (num - k) `mod` 100
       in (num', acc + pZero)
    go (num, acc) ('R' : sNum) =
      let k = parseInt sNum
          pZero = passedZeroR num k
          num' = (num + k) `mod` 100
       in (num', acc + pZero)
    go _ _ = error "didn't expect to end here"

    passedZeroL start = passedZero $ if start == 0 then 100 else start

    passedZeroR start = passedZero $ if start == 0 then 100 else 100 - start

    passedZero start' k
      | k < start' = 0
      | otherwise = 1 + (k - start') `div` 100
