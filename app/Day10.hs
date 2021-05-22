module Day10 (day10) where

import System.IO
import Control.Exception (assert)
import Data.Function ((&))
import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

day10 :: IO ()
day10 = withFile "data/10.txt" ReadMode \fin -> do
    c <- TIO.hGetContents fin
    let xs' :: [Int] = sort $ map (read . T.unpack) $ T.lines c
    let xs = 0:xs' ++ [3 + last xs']

    let ds = [y - x | (x, y) <- zip xs (tail xs)]
            & map \d -> assert (1 <= d && d <= 3) d
    let num1 = length $ filter (== 1) ds
    let num3 = length $ filter (== 3) ds
    putStr "part 1: "
    print $ num1 * num3
    putStr "part 2: "
    print $ head $ dp xs

dp :: [Int] -> [Int]
dp [] = undefined
dp [x] = [1]
dp (x:xs) = let
    ys = dp xs
    xs' = takeWhile (<= x + 3) xs
    y = sum $ take (length xs') ys
    in y:ys
