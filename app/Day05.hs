module Day05 (day05) where

import System.IO
import Control.Exception (assert)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace

day05 :: IO ()
day05 = withFile "data/05.txt" ReadMode \fin -> do
    c <- TIO.hGetContents fin
    let lines = T.lines c
    let seats = sort [r * 8 + c | (r, c) <- map parseSeat lines]

    putStr "part 1: "
    print $ last seats

    let gaps = [(a, b) | (a, b) <- zip seats (tail seats), a + 1 /= b]
    let [(before, after)] = gaps
    putStr "part 2: "
    print $ assert (before + 2 == after) (before + 1)

parseSeat :: Text -> (Int, Int)
parseSeat s = let
    s' = assert (T.length s == 10) s
    row = T.take 7 s'
    column = T.drop 7 s'
    in
    (parseBinary 'F' 'B' row, parseBinary 'L' 'R' column)

parseBinary :: Char -> Char -> Text -> Int
parseBinary zero one s = parseRev $ reverse $ T.unpack s
    where
        parseRev [] = 0
        parseRev (c:cs) = 2 * parseRev cs + if
            | c == zero -> 0
            | c == one -> 1
            | otherwise -> undefined
