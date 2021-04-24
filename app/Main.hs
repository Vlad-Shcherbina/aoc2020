{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.IO

main :: IO ()
main = do
    fin <- openFile "data/01.txt" ReadMode
    c <- hGetContents fin
    let xs :: [Int] = map read $ lines c

    let result = [x * y | x <- xs, y <- xs, x + y == 2020]
    putStrLn $ show result
    
    let result2 = [x * y * z | x <- xs, y <- xs, z <- xs, x + y + z == 2020]
    putStrLn $ show result2
