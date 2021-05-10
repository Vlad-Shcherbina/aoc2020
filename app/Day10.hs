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
    let xs :: [Int] = sort $ map (read . T.unpack) $ T.lines c

    let ds = [y - x | (x, y) <- zip (0:xs) (xs ++ [3 + last xs])]
            & map \d -> assert (1 <= d && d <= 3) d
    let num1 = length $ filter (== 1) ds
    let num3 = length $ filter (== 3) ds
    putStr "part 1: "
    print $ num1 * num3
