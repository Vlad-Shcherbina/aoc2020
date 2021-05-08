module Day05 (day05) where

import System.IO
import Control.Exception (assert)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace

day05 :: IO ()
day05 = withFile "data/05.txt" ReadMode \fin -> do
    c <- TIO.hGetContents fin
    let lines = T.lines c
    putStr "part 1: "
    print $ maximum [r * 8 + c | (r, c) <- map parseSeat lines]

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
