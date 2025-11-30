module Utils.Grid.Matrix (
  GridPos,
  Grid,
  CharGrid,
  gridify,
  gridifyFromText,
  cardinalNeighborsPos,
  diagonalNeighborsPos,
  allPositions,
  allNeighborsPos,
  allNeighbors,
  renderCharGrid,
  (!),
  bounds,
  inBounds,
  setGridElem,
)
where

import Data.Matrix (
  Matrix (..),
  fromLists,
  getElem,
  prettyMatrix,
  setElem,
  submatrix,
 )
import Data.Text (Text, unpack)

type GridPos = (Int, Int)

type Grid a = Matrix a

type CharGrid = Grid Char

gridify :: [[a]] -> Grid a
gridify = fromLists

gridifyFromText :: [Text] -> CharGrid
gridifyFromText = gridify . map unpack

allPositions :: Grid a -> [GridPos]
allPositions grid =
  let (m, n) = bounds grid in [(i, j) | i <- [0 .. m - 1], j <- [0 .. n - 1]]

cardinalNeighborsPos :: GridPos -> [GridPos]
cardinalNeighborsPos (i, j) = [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]

diagonalNeighborsPos :: GridPos -> [GridPos]
diagonalNeighborsPos (i, j) =
  [(i - 1, j - 1), (i - 1, j + 1), (i + 1, j - 1), (i + 1, j + 1)]

allNeighborsPos :: GridPos -> [GridPos]
allNeighborsPos p = cardinalNeighborsPos p ++ diagonalNeighborsPos p

allNeighbors :: Grid a -> GridPos -> Grid a
allNeighbors grid (i, j) =
  let (m, n) = bounds grid
   in submatrix (max 1 i) (min m (i + 2)) (max 1 j) (min n (j + 2)) grid

-- Zero-indexing.
(!) :: Grid a -> GridPos -> a
(!) grid (i, j) = getElem (i + 1) (j + 1) grid

setGridElem :: a -> GridPos -> Grid a -> Grid a
setGridElem elm (i, j) = setElem elm (i + 1, j + 1)

bounds :: Grid a -> (Int, Int)
bounds grid = (nrows grid, ncols grid)

inBounds :: Grid a -> GridPos -> Bool
inBounds grid (i, j) =
  let (m, n) = bounds grid in i >= 0 && j >= 0 && i <= m - 1 && j <= n - 1

renderCharGrid :: CharGrid -> String
renderCharGrid = prettyMatrix
