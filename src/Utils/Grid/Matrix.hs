module Utils.Grid.Matrix (
  GridPos,
  Grid (..),
  CharGrid,
  gridify,
  gridifyFromText,
  cardinalNeighborsPos,
  diagonalNeighborsPos,
  allNeighborsPos,
  allNeighbors,
  renderCharGrid,
  (!),
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

data Grid a = Grid
  { gridNumRows :: Int
  , gridNumCols :: Int
  , gridMatrix :: Matrix a
  , gridPositions :: [GridPos]
  , gridBounds :: (Int, Int)
  }

type CharGrid = Grid Char

gridify :: [[a]] -> Grid a
gridify lsts =
  Grid
    { gridNumRows = m
    , gridNumCols = n
    , gridMatrix = grid
    , gridPositions = [(i, j) | i <- [0 .. m - 1], j <- [0 .. n - 1]]
    , gridBounds = (m, n)
    }
  where
    grid = fromLists lsts
    m = nrows grid
    n = ncols grid

gridifyFromText :: [Text] -> CharGrid
gridifyFromText = gridify . map unpack

inBounds :: Grid a -> GridPos -> Bool
inBounds grid (i, j) = i >= 0 && j >= 0 && i <= m - 1 && j <= n - 1
  where
    (m, n) = gridBounds grid

renderCharGrid :: CharGrid -> String
renderCharGrid (Grid {gridMatrix = g}) = prettyMatrix g

allNeighbors :: Grid a -> GridPos -> [a]
allNeighbors grid = map (grid !) . allNeighborsPos

setGridElem :: a -> GridPos -> Grid a -> Grid a
setGridElem elm (i, j) grid@(Grid {gridMatrix = g}) =
  grid {gridMatrix = setElem elm (i + 1, j + 1) g}

(!) :: Grid a -> GridPos -> a
(!) (Grid {gridMatrix = g}) (i, j) = getElem (i + 1) (j + 1) g

cardinalNeighborsPos :: GridPos -> [GridPos]
cardinalNeighborsPos (i, j) = [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]

diagonalNeighborsPos :: GridPos -> [GridPos]
diagonalNeighborsPos (i, j) =
  [(i - 1, j - 1), (i - 1, j + 1), (i + 1, j - 1), (i + 1, j + 1)]

allNeighborsPos :: GridPos -> [GridPos]
allNeighborsPos p = cardinalNeighborsPos p ++ diagonalNeighborsPos p
