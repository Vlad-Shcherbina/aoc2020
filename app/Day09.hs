module Day09 (day09) where

import System.IO
import Data.List (tails, scanl')
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

day09 :: IO ()
day09 = withFile "data/09.txt" ReadMode \fin -> do
    c <- TIO.hGetContents fin
    let xs :: [Int] = map (read . T.unpack) $ T.lines c

    let groups = [(take 25 xs, xs !! 25) | xs <- tails xs, length xs > 25]

    putStr "part 1: "
    let part1 = head [s | (xs, s) <- groups, not $ isSum xs s]
    print part1

    putStr "part 2: "
    let [ys] = [ys | ys <- infixesWithSum xs part1, length ys > 1]
    print $ minimum ys + maximum ys

isSum :: [Int] -> Int -> Bool
isSum xs sum = go S.empty xs where
    go s [] = False
    go s (x:xs) = (sum - x) `S.member` s || go (S.insert x s) xs

prefixesWithSum :: [Int] -> Int -> [[Int]]
prefixesWithSum xs sum = do
    (len, s) <- scanl' (\(len, s) x -> (len + 1, s + x)) (0, 0) xs
    if s == sum
        then return $ take len xs
        else []

infixesWithSum :: [Int] -> Int -> [[Int]]
infixesWithSum xs sum = do
    t <- tails xs
    prefixesWithSum t sum
