module Day02 (day02) where

import System.IO
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

day02 :: IO ()
day02 = do
    c <- withFile "data/02.txt" ReadMode TIO.hGetContents
    let pls = map parse $ T.lines $ c

    print $ length $ filter isValid pls
    print $ length $ filter isValid2 pls

isValid :: ParsedLine -> Bool
isValid (ParsedLine {..}) = let
    cnt = T.count (T.singleton ch) pwd
    in
        minCnt <= cnt && cnt <= maxCnt

isValid2 :: ParsedLine -> Bool
isValid2 (ParsedLine {..}) =
    (T.index pwd (minCnt - 1) == ch) /=
    (T.index pwd (maxCnt - 1) == ch)

data ParsedLine = ParsedLine
    { minCnt :: Int
    , maxCnt :: Int
    , ch :: Char
    , pwd :: Text
    } deriving (Show)

parse :: Text -> ParsedLine
parse line = let
    Just (policy, pwd) = partition ": " line
    Just (range, chStr) = partition " " policy
    Just (minStr, maxStr) = partition "-" range
    minCnt = read $ T.unpack minStr
    maxCnt = read $ T.unpack maxStr
    [ch] = T.unpack chStr
    in
        ParsedLine { minCnt, maxCnt, ch, pwd }

-- partition "/" "a/b/c" = Just ("a", "b/c")
partition :: Text -> Text -> Maybe (Text, Text)
partition sep s = case T.breakOnAll sep s of
    (left, right):_ -> case T.stripPrefix sep right of
        Just right -> Just (left, right)
    [] -> Nothing
