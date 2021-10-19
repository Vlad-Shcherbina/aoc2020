module Day16 (day16) where

import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (transpose)
import Data.List.Extra (minimumOn)
import Util (partition)

day16 :: IO ()
day16 = withFile "data/16.txt" ReadMode \fin -> do
    c <- TIO.hGetContents fin
    let lines = T.lines c
    let (rules', rest) = span ( /= "") lines
    let "" : "your ticket:" : myTicket' : "" : "nearby tickets:" : nearby' = rest
    let rules = map parseRule rules'
    let myTicket = parseTicket myTicket'
    let nearby = map parseTicket nearby'

    let allRanges = concatMap ranges rules
    let allFields = concat nearby
    let isValid = inRanges allRanges

    putStr "part 1: "
    print $ sum $ filter (not . isValid) allFields

    let validNearby = filter (all isValid) nearby
    let columns = transpose validNearby
    let matchingRules column = [name | Rule { name, ranges } <- rules, all (inRanges ranges) column]
    let [sol] = solveConstraints $ zip [0..] (map matchingRules columns)

    putStr "part 2: "
    print $ product [myTicket !! i | (i, name) <- sol, T.isPrefixOf "departure" name]

solveConstraints :: (Eq k, Eq a) => [(k, [a])] -> [[(k, a)]]
solveConstraints [] = pure []
solveConstraints constraints = do
    let (k, as) = minimumOn (\(k, as) -> length as) constraints
    a <- as
    let constraints' = [(k', filter (/= a) as') | (k', as') <- constraints, k' /= k]
    sol <- solveConstraints constraints'
    pure $ (k, a) : sol

inRanges :: [(Int, Int)] -> Int -> Bool
inRanges ranges x = or [l <= x && x <= r | (l, r) <- ranges]

parseTicket :: T.Text -> [Int]
parseTicket s = map (read . T.unpack) $ T.split (==',') s

data Rule = Rule { name :: T.Text, ranges :: [(Int, Int)] } deriving (Show)
parseRule :: T.Text -> Rule
parseRule s = let
    Just (name, rs) = partition ": " s
    rs' = T.splitOn " or " rs
    parseRange :: T.Text -> (Int, Int)
    parseRange s = let
        Just (l, r) = partition "-" s
        in (read $ T.unpack l, read $ T.unpack r)
    ranges = map parseRange rs'
    in Rule {..}
