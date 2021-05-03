module Main where

import System.IO
import System.Environment
import System.Exit

import Day01 (day01)
import Day02 (day02)
import Day03 (day03)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["1"] -> day01
        ["2"] -> day02
        ["3"] -> day03
        _ -> do
            putStrLn "Usage: cabal run aoc2020 -- <day>"
            exitWith $ ExitFailure 1
