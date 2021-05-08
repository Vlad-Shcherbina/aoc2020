module Day06 (day06) where

import System.IO
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

day06 :: IO ()
day06 = withFile "data/06.txt" ReadMode \fin -> do
    c <- TIO.hGetContents fin
    let groups = splitWhen T.null $ T.lines c
    let group = head groups
    let count group = Set.size $ Set.fromList $ concat $ map T.unpack group
    putStr "part 1: "
    print $ sum $ map count groups

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p [] = [[]]
splitWhen p (x:xs) =
    let
        g:gs = splitWhen p xs
    in if p x
        then []:g:gs
        else (x:g):gs
