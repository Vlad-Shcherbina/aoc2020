module Day17 (day17) where

import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)

day17 :: IO ()
day17 = withFile "data/17.txt" ReadMode \fin -> do
    c <- TIO.hGetContents fin
    let
        lines = T.lines c
        cells2d = [(i, j) |
            (i, line) <- zip [0..] lines,
            (j, c) <- zip [0..] (T.unpack line),
            c == '#']

    let cells3d = Map.fromList [ ([0, i, j], ()) | (i, j) <- cells2d]
    putStr "part 1: "
    print $ length $ Map.toList $ iterate step cells3d !! 6

    let cells4d = Map.fromList [ ([0, 0, i, j], ()) | (i, j) <- cells2d]
    putStr "part 2: "
    print $ length $ Map.toList $ iterate step cells4d !! 6

neighbors :: [Int] -> [[Int]]
neighbors pos = filter (/= pos) $ mapM (\x -> [x - 1 .. x + 1]) pos

neighborCount :: Map.Map [Int] () -> Map.Map [Int] Int
neighborCount cells = Map.fromListWith (+) $ map (,1) $ concatMap neighbors $ Map.keys cells

cellStep :: Bool -> Int -> Bool
cellStep _ 3 = True
cellStep cur 2 = cur
cellStep _ _ = False

step cells = Map.fromList [
    (pos, ()) |
    (pos, cnt) <- Map.toList $ neighborCount cells,
    cellStep (isJust $ Map.lookup pos cells) cnt]
