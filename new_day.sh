#!/usr/bin/env bash

# Exit on error
set -e

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <day-number>"
    exit 1
fi

# Format day number with leading zero (e.g., 3 -> 03)
DAY=$(printf "%02d" "$1")
DAYNAME="day${DAY}"
FILENAME="solutions/Day${DAY}.hs"

if [ -e "$FILENAME" ]; then
    echo "File $FILENAME already exists."
    exit 1
fi

# Create required directories
mkdir -p solutions
mkdir -p data/inputs
mkdir -p data/tests

# Create DayNN.hs
cat >"$FILENAME" <<EOF
import Data.Heap (Heap)
import qualified Data.Heap as H
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map (Map)
import qualified Data.Map as M
import Data.Matrix hiding ((!))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.Environment (getArgs)
import Utils.Grid.Matrix (GridPos, Grid, CharGrid, (!))
import qualified Utils.Grid.Matrix as G
import Utils.InputProcessing

type Solver = ProcessedInput -> Int

day :: String
day = "${DAY}"

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
EOF

# Create empty input files
touch "data/inputs/day${DAY}.txt"
touch "data/tests/day${DAY}.txt"

# Append executable stanza to .cabal file if not already present
CABAL_FILE=$(find . -maxdepth 1 -name "*.cabal" | head -n 1)

if grep -q "^executable ${DAYNAME}$" "$CABAL_FILE"; then
    echo "Executable '${DAYNAME}' already exists in ${CABAL_FILE}."
else
    echo "" >>"$CABAL_FILE"
    cat >>"$CABAL_FILE" <<EOF
executable ${DAYNAME}
    import:           warnings
    main-is:          Day${DAY}.hs
    hs-source-dirs:   solutions
    default-language: Haskell2010
    default-extensions: OverloadedStrings
    build-depends:    
        base
        , aoc25
        , text
        , vector
        , split
        , containers
        , mtl
        , regex-tdfa
        , heaps
        , matrix
EOF
    echo "Added 'executable ${DAYNAME}' to ${CABAL_FILE}"
fi

echo "Created:"
echo "  $FILENAME"
echo "  data/inputs/day${DAY}.txt"
echo "  data/tests/day${DAY}.txt"
