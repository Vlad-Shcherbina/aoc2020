module Day06 (day06) where

import System.IO
import Data.List (foldl1')
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

day06 :: IO ()
day06 = withFile "data/06.txt" ReadMode \fin -> do
    c <- TIO.hGetContents fin
    let rawGroups = splitWhen T.null $ T.lines c
    let rawToGroup raw = map (Set.fromList . T.unpack) raw
    let groups = map rawToGroup rawGroups

    putStr "part 1: "
    print $ sum $ map (Set.size . Set.unions) groups
    
    putStr "part 2: "
    let intersections = foldl1' Set.intersection
    print $ sum $ map (Set.size . intersections) groups

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p [] = [[]]
splitWhen p (x:xs) =
    let
        g:gs = splitWhen p xs
    in if p x
        then []:g:gs
        else (x:g):gs
