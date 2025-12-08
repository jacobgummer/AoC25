import Control.Monad
import Control.Monad.ST (runST)
import Data.List (sort)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.UnionFind.ST (equivalent, getWeight, makeSet, union)
import System.Environment (getArgs)
import Utils.InputProcessing (readInputLines, readIntsBy)

type Solver = ProcessedInput -> Int

day :: String
day = "08"

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

type JBoxPos = (Int, Int, Int)

type ProcessedInput = [JBoxPos]

procesInput :: [String] -> ProcessedInput
procesInput = map (tuplify . readIntsBy ",")
  where
    tuplify [x, y, z] = (x, y, z)
    tuplify _ = undefined

runInput :: FilePath -> Solver -> IO ()
runInput file part = do
  raw <- readInputLines file
  let input = procesInput raw
  print $ part input

distSquared :: JBoxPos -> JBoxPos -> Int
distSquared (x1, y1, z1) (x2, y2, z2) =
  sq (x1 - x2) + sq (y1 - y2) + sq (z1 - z2)
  where
    sq x = x * x

part1 :: Solver
part1 jBoxes = product biggest3
  where
    pairsSorted =
      map snd $
        sort
          [ (distSquared jb1 jb2, (jb1, jb2))
          | jb1 <- jBoxes
          , jb2 <- jBoxes
          , jb1 > jb2
          ]

    k = 1000 :: Int

    biggest3 = runST $ do
      psMap <- M.fromList . zip jBoxes <$> mapM makeSet jBoxes
      forM_ (take k pairsSorted) $ \(jb1, jb2) -> do
        let p1 = psMap M.! jb1
            p2 = psMap M.! jb2
        areEquiv <- equivalent p1 p2
        unless areEquiv $ p1 `union` p2
      allCircuitSizes <- mapM getWeight $ M.elems psMap
      pure $ take 3 $ S.toDescList $ S.fromList allCircuitSizes

part2 :: Solver
part2 jBoxes = res
  where
    n = length jBoxes

    pairsSorted =
      map snd $
        sort
          [ (distSquared jb1 jb2, (jb1, jb2))
          | jb1 <- jBoxes
          , jb2 <- jBoxes
          , jb1 > jb2
          ]

    xCord (x, _, _) = x

    connectAll _ [] _ = error "didn't connect all junction boxes"
    connectAll psMap ((jb1, jb2) : pairs') lastProd = do
      let p1 = psMap M.! jb1
          p2 = psMap M.! jb2
      w <- getWeight p1
      if w == n
        then pure lastProd
        else do
          areEquiv <- equivalent p1 p2
          unless areEquiv $ p1 `union` p2
          connectAll psMap pairs' (xCord jb1 * xCord jb2)

    res = runST $ do
      psMap <- M.fromList . zip jBoxes <$> mapM makeSet jBoxes
      connectAll psMap pairsSorted 1
