module Main where

import System.IO
import System.Environment
import System.Exit

import Day02 (day02)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["1"] -> day01
        ["2"] -> day02
        otherwise -> do
            putStrLn "Usage: cabal run aoc2020 -- <day>"
            exitWith $ ExitFailure 1

day01 :: IO ()
day01 = do
    fin <- openFile "data/01.txt" ReadMode
    c <- hGetContents fin
    let xs :: [Int] = map read $ lines c

    putStrLn $ show $ head [x * y | x <- xs, y <- xs, x + y == 2020]    
    putStrLn $ show $ head [x * y * z | x <- xs, y <- xs, z <- xs, x + y + z == 2020]
