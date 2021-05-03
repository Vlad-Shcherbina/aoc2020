module Day03 (day03) where

import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

slopeRight = 3

day03 :: IO ()
day03 = withFile "data/03.txt" ReadMode \fin -> do
    c <- TIO.hGetContents fin
    let lines = T.lines c

    let trace = [T.index line (i * slopeRight `mod` T.length line) | (i, line) <- zip [0..] lines]
    putStr "part 1: "
    print $ length $ filter (== '#') trace
