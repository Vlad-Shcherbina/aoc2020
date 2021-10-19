module Day17 (day17) where

import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Map.Strict as Map
import Data.Maybe (isJust)

day17 :: IO ()
day17 = withFile "data/17.txt" ReadMode \fin -> do
    c <- TIO.hGetContents fin
    let lines = T.lines c
    let
        cells = Map.fromList [ ((0, i, j), ()) |
            (i, line) <- zip [0..] lines,
            (j, c) <- zip [0..] (T.unpack line),
            c == '#']
    putStr "part 1: "
    print $ length $ Map.toList $ iterate step cells !! 6

neighborCount :: Map.Map (Int, Int, Int) () -> Map.Map (Int, Int, Int) Int
neighborCount cells = fromListWith (+) [((i', j', k'), 1) |
    ((i, j, k), ()) <- Map.toList cells,
    i' <- [i - 1 .. i + 1],
    j' <- [j - 1 .. j + 1],
    k' <- [k - 1 .. k + 1],
    (i, j, k) /= (i', j', k')]

cellStep :: Bool -> Int -> Bool
cellStep _ 3 = True
cellStep cur 2 = cur
cellStep _ _ = False

step cells = Map.fromList [
    (pos, ()) |
    (pos, cnt) <- Map.toList $ neighborCount cells,
    cellStep (isJust $ Map.lookup pos cells) cnt]
