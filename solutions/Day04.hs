import System.Environment (getArgs)
import Utils.Grid.Matrix (CharGrid, GridPos, (!))
import qualified Utils.Grid.Matrix as G
import Utils.InputProcessing (readInputLines)

type Solver = ProcessedInput -> Int

day :: String
day = "04"

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

checkPos :: CharGrid -> GridPos -> Int
checkPos g p = if g ! p == '.' then 0 else res
  where
    neighboursLst = map (g !) $ filter (G.inBounds g) $ G.allNeighborsPos p

    res =
      if length (filter (== '@') neighboursLst) < 4
        then 1
        else 0

part1 :: Solver
part1 input = sum $ map (checkPos input) $ G.allPositions input

determineAllToBeRemoved :: CharGrid -> [GridPos] -> [GridPos] -> [GridPos]
determineAllToBeRemoved _ [] acc = acc
determineAllToBeRemoved g (p : pos') acc =
  if g ! p == '.'
    then determineAllToBeRemoved g pos' acc
    else
      determineAllToBeRemoved g pos' $
        if length (filter (== '@') neighboursLst) < 4
          then p : acc
          else acc
  where
    neighboursLst = map (g !) $ filter (G.inBounds g) $ G.allNeighborsPos p

removePaperRolls :: CharGrid -> [GridPos] -> CharGrid
removePaperRolls = foldl $ flip $ G.setGridElem '.'

part2 :: Solver
part2 = transform 0
  where
    transform acc g' =
      let toBeRemoved = determineAllToBeRemoved g' (G.allPositions g') []
          numRemoved = length toBeRemoved
          g'' = removePaperRolls g' toBeRemoved
       in if numRemoved == 0
            then acc
            else transform (acc + numRemoved) g''
