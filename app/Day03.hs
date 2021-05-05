module Day03 (day03) where

import System.IO
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

slopeRight = 3  -- part 1
slopes = [1, 3, 5, 7]  -- part 2, except (1, 2)

day03 :: IO ()
day03 = withFile "data/03.txt" ReadMode \fin -> do
    c <- TIO.hGetContents fin
    let lines = T.lines c

    putStr "part 1: "
    print $ treesOnTrace lines slopeRight

    let treeCounts = treesOnTrace (everyOther lines) 1 :
                     map (treesOnTrace lines) slopes
    putStr "part 2: "
    print $ product treeCounts

treesOnTrace :: [Text] -> Int -> Int
treesOnTrace lines slope =
    let trace = [T.index line (i * slope `mod` T.length line) 
                 | (i, line) <- zip [0..] lines]
    in length $ filter (== '#') trace

everyOther :: [a] -> [a]
everyOther (x:y:xs) = x : everyOther xs
everyOther xs = xs
