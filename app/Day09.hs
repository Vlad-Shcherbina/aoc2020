module Day09 (day09) where

import System.IO
import Data.List (tails)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

day09 :: IO ()
day09 = withFile "data/09.txt" ReadMode \fin -> do
    c <- TIO.hGetContents fin
    let xs :: [Int] = map (read . T.unpack) $ T.lines c

    let groups = [(take 25 xs, xs !! 25) | xs <- tails xs, length xs > 25]

    putStr "part 1: "
    print $ head [s | (xs, s) <- groups, not $ isSum xs s]

isSum :: [Int] -> Int -> Bool
isSum xs sum = go S.empty xs where
    go s [] = False
    go s (x:xs) = (sum - x) `S.member` s || go (S.insert x s) xs
