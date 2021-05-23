module Day11 (day11) where

import System.IO
import Data.Function ((&))
import Data.List (delete)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Vector (Vector)
import qualified Data.Vector as V

day11 :: IO ()
day11 = withFile "data/11.txt" ReadMode \fin -> do
    c <- TIO.hGetContents fin
    let grid = V.fromList $ map parseRow $ T.lines c
    putStr "part 1: "
    print $ solve (neighbors, 4) grid
    putStr "part 2: "
    print $ solve (losNeighbors, 5) grid

type Neighbors = Grid -> Int -> Int -> [Cell]
type Rules = (Neighbors, Int)

solve :: Rules -> Grid -> Int
solve rules grid = let
    grids = iterate (step rules) grid
    (stableGrid, _) = head $ filter (uncurry (==)) $ zip grids (tail grids)
    in
    concatMap V.toList (V.toList stableGrid) & filter (== TakenSeat) & length

step :: Rules -> Grid -> Grid
step (neighbors, threshold) g = V.imap (V.imap . f) g
    where
        f i j Floor = Floor
        f i j EmptySeat = if TakenSeat `notElem` neighbors g i j
            then TakenSeat
            else EmptySeat
        f i j TakenSeat = if length (filter (== TakenSeat) (neighbors g i j)) >= threshold
            then EmptySeat
            else TakenSeat

neighbors :: Grid -> Int -> Int -> [Cell]
neighbors g i j = mapMaybe f offsets
    where f (di, dj) = gridGet g (i + di) (j + dj)

losNeighbors :: Grid -> Int -> Int -> [Cell]
losNeighbors g i j = mapMaybe f offsets
    where f (di, dj) = head [
            c | k <- [1..],
                let c = gridGet g (i + k * di) (j + k * dj),
                c /= Just Floor]

gridGet :: Grid -> Int -> Int -> Maybe Cell
gridGet g i j = do
    row <- g V.!? i
    row V.!? j

offsets :: [(Int, Int)]
offsets = delete (0, 0) [(i, j) | i <- [-1..1], j <- [-1..1]]

data Cell = Floor | EmptySeat | TakenSeat  deriving (Eq, Show)
type Grid = Vector (Vector Cell)

parseRow :: Text -> Vector Cell
parseRow s = V.generate (T.length s) (parseCell . T.index s)

parseCell :: Char -> Cell
parseCell '.' = Floor
parseCell 'L' = EmptySeat
parseCell '#' = TakenSeat
parseCell _ = undefined
