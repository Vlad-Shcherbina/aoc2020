module Day07 (day07) where

import System.IO
import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Util

day07 :: IO ()
day07 = withFile "data/07.txt" ReadMode \fin -> do
    c <- TIO.hGetContents fin
    let rules = map parseRule $ T.lines c
    let parents = collectToMapOfLists
            [(right, left) | (left, rights) <- rules, (_, right) <- rights]

    putStr "part 1: "
    print $ Set.size $ reachable parents "shiny gold"
    putStr "part 2: "
    print $ numBagsInside (Map.fromList rules) "shiny gold"

reachable :: (Ord a, Show a) => Map a [a] -> a -> Set a
reachable graph start = loop (graph Map.! start) Set.empty
    where
        loop [] visited = visited
        loop (x:xs) visited = if x `Set.member` visited
            then loop xs visited
            else let
                next = fromMaybe [] $ Map.lookup x graph
                visited' = Set.insert x visited
                in loop (next ++ xs) visited'

numBagsInside :: Map Text [(Int, Text)] -> Text -> Int
numBagsInside rules bag =
    sum [cnt * (1 + numBagsInside rules b) | (cnt, b) <- rules Map.! bag]

collectToMapOfLists :: Ord k => [(k, v)] -> Map k [v]
collectToMapOfLists kvs = Map.fromListWith (++) [(k, [v]) | (k, v) <- kvs]

parseRule :: Text -> (Text, [(Int, Text)])
parseRule s = let
    Just s' = T.stripSuffix "." s
    Just (left, right) = partition " contain " s'
    Just left' = T.stripSuffix " bags" left
    right' = case right of
        "no other bags" -> []
        _ -> map parseBags $ T.splitOn ", " right
    in
    (left', right')

parseBags :: Text -> (Int, Text)
parseBags s = let
    Just (cnt, bags) = partition " " s
    cnt' = read $ T.unpack cnt
    Just bags' = case  cnt' of
        1 -> T.stripSuffix " bag" bags
        _ -> T.stripSuffix " bags" bags
    in
    (cnt', bags')
