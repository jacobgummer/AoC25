{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}
import Data.Bifunctor (Bifunctor (first))
import Data.Char (isSpace)
import Data.Matrix (
  Matrix (ncols, nrows),
  getCol,
  getRow,
  submatrix,
  transpose,
 )
import Data.Maybe (fromMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.Environment (getArgs)
import Utils.Grid.Matrix (CharGrid)
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

type ProcessedInput = (G.Grid String, CharGrid)

procesInput :: [String] -> ProcessedInput
procesInput strs =
  (transpose $ G.gridify $ map words strs, transpose $ G.gridify strs)

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
part1 (m, _) = res
  where
    res = foldr checkRow 0 [1 .. nrows m]

    checkRow i acc =
      let row = getRow i m
          (vecNums, op) =
            first (V.map parseInt) $ fromMaybe undefined $ V.unsnoc row
       in acc + operate op vecNums

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

getOperator :: String -> (Int -> Int -> Int)
getOperator "+" = (+)
getOperator "*" = (*)
getOperator _ = undefined

part2 :: Solver
part2 (_, m) = res' + lastCol
  where
    nRows = nrows m
    nCols = ncols m
    ops = words $ V.foldr (:) "" $ getCol nCols m
    matNums = submatrix 1 nRows 1 (nCols - 1) m

    (_, lastCol, res') =
      foldl checkRow (0, if ops !! 0 == "+" then 0 else 1, 0) [1 .. nRows]

    checkRow (i, colAcc, acc) row =
      let rowVec = getRow row matNums
          num' = trim $ V.foldr (:) "" rowVec
          op = getOperator $ ops !! i
       in if not $ null num'
            then (i, colAcc `op` parseInt num', acc)
            else
              ( i + 1
              , if ops !! (i + 1) == "+" then 0 else 1
              , acc + colAcc
              )
