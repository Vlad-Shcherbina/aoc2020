module Day04 (day04) where

import System.IO
import Control.Exception (assert)
import Data.Maybe (fromJust)
import Data.List (delete)
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Util

import Debug.Trace

fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]

day04 :: IO ()
day04 = withFile "data/04.txt" ReadMode \fin -> do
    c <- TIO.hGetContents fin
    let passports = T.splitOn "\n\n" c 
                    & map parsePassport
    putStr "part 1: "
    print $ length $ filter (hasAllFields (delete "cid" fields)) passports

parsePassport :: Text -> [(Text, Text)]
parsePassport p = p
    & T.split (\c -> c == ' ' || c == '\n')
    & filter (/= "")
    & map (fromJust . partition ":")

hasAllFields :: [Text] -> [(Text, Text)] -> Bool
hasAllFields fields p =
    all (`elem` pFields) fields
    where pFields = map fst p
