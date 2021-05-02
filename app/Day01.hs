module Day01 (day01) where

import System.IO

day01 :: IO ()
day01 = do
    fin <- openFile "data/01.txt" ReadMode
    c <- hGetContents fin
    let xs :: [Int] = map read $ lines c

    print $ head [x * y | x <- xs, y <- xs, x + y == 2020]    
    print $ head [x * y * z | x <- xs, y <- xs, z <- xs, x + y + z == 2020]
