module Day02 (day02) where

import System.IO
import qualified Data.Text as T
import Data.Text (Text)

day02 :: IO ()
day02 = do
    fin <- openFile "data/02.txt" ReadMode
    c <- hGetContents fin
    let pls = map parse $ T.lines $ T.pack c
    putStrLn $ show $ length [() | pl <- pls, isValid pl]

data ParsedLine = ParsedLine {
    min_cnt :: Int,
    max_cnt :: Int,
    ch :: Char,
    pwd :: Text
} deriving (Show)

isValid (ParsedLine { min_cnt, max_cnt, ch, pwd }) = let
    cnt = T.count (T.singleton ch) pwd
    in
        min_cnt <= cnt && cnt <= max_cnt

parse :: Text -> ParsedLine
parse line = let
    Just (policy, pwd) = partition ": " line
    Just (range, ch_str) = partition " " policy
    Just (min_str, max_str) = partition "-" range
    min_cnt :: Int = read $ T.unpack min_str
    max_cnt :: Int = read $ T.unpack max_str
    [ch] = T.unpack ch_str
    in
        ParsedLine { min_cnt, max_cnt, ch, pwd }

-- partition "/" "a/b/c" = Just ("a", "b/c")
partition :: Text -> Text -> Maybe (Text, Text)
partition sep s = case T.breakOnAll sep s of
    (left, right):_ -> case T.stripPrefix sep right of
        Just right -> Just (left, right)
    [] -> Nothing
