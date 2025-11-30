module Utils.Grid.Map
  ( Grid,
    GridPos,
    CharGrid,
    gridify,
    bounds,
    cardinalNeighbors,
    diagonalNeighbors,
    allNeighbors,
    inBounds,
    inBounds00,
    renderCharGrid,
    gridifyFromText,
  )
where

import qualified Data.Map as M
import Data.Text (Text, unpack)

type GridPos = (Int, Int)

type Grid a = M.Map GridPos a

type CharGrid = Grid Char

gridify :: [[a]] -> Grid a
gridify rows =
  let positions =
        [ ((i, j), x)
        | (i, row) <- zip [0 ..] rows,
          (j, x) <- zip [0 ..] row
        ]
   in M.fromList positions

-- | Probably a good idea to save this somewhere in
-- the program using it.
bounds :: Grid a -> (GridPos, GridPos)
bounds grid =
  let ks = M.keys grid
      is = map fst ks
      js = map snd ks
   in ((minimum is, minimum js), (maximum is, maximum js))

cardinalNeighbors :: GridPos -> [GridPos]
cardinalNeighbors (i, j) = [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]

diagonalNeighbors :: GridPos -> [GridPos]
diagonalNeighbors (i, j) =
  [(i - 1, j - 1), (i - 1, j + 1), (i + 1, j - 1), (i + 1, j + 1)]

allNeighbors :: GridPos -> [GridPos]
allNeighbors p = cardinalNeighbors p ++ diagonalNeighbors p

inBounds :: Grid a -> GridPos -> Bool
inBounds grid (i, j) =
  let ((imin, jmin), (imax, jmax)) = bounds grid
   in i >= imin && j >= jmin && i <= imax && j <= jmax

inBounds00 :: Grid a -> GridPos -> Bool
inBounds00 grid (i, j) =
  let (m, n) = snd $ bounds grid in i >= 0 && j >= 0 && i <= m && j <= n

renderCharGrid :: CharGrid -> String
renderCharGrid grid =
  let coords = M.keys grid
      maxRow = maximum $ map fst coords
      maxCol = maximum $ map snd coords
   in unlines
        [ [ M.findWithDefault '.' (i, j) grid
          | j <- [0 .. maxCol]
          ]
        | i <- [0 .. maxRow]
        ]

gridifyFromText :: [Text] -> CharGrid
gridifyFromText = gridify . map unpack
