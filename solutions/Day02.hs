import Data.Bifunctor (Bifunctor (second))
import Data.List (isInfixOf)
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
type ProcessedInput = [(String, String)]

procesInput :: String -> ProcessedInput
procesInput = map (second (drop 1) . breakLine '-') . splitOn ","

runInput :: FilePath -> Solver -> IO ()
runInput file part = do
  (raw : _) <- readInputLines file
  let input = procesInput raw
  print $ part input

checkRange1 :: (String, String) -> Int
checkRange1 (start, end) = sum invalids
  where
    startInt = parseInt start
    endInt = parseInt end

    invalids =
      map parseInt $
        filter repeatsTwice $
          map show [startInt .. endInt]

    repeatsTwice s = repeatsTwice' s $ take (length s `div` 2) s

    repeatsTwice' s s' = s == s' ++ s'

part1 :: Solver
part1 = sum . map checkRange1

checkRange2 :: (String, String) -> Int
checkRange2 (start, end) = sum invalids
  where
    startInt = parseInt start
    endInt = parseInt end

    invalids =
      map parseInt $
        filter repeats $
          map show [startInt .. endInt]

    repeats s = s `isInfixOf` (init . drop 1 $ s ++ s)

part2 :: Solver
part2 = sum . map checkRange2
