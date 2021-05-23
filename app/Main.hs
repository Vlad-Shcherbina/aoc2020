module Main where

import System.IO
import System.Environment
import System.Exit

import Day01 (day01)
import Day02 (day02)
import Day03 (day03)
import Day04 (day04)
import Day05 (day05)
import Day06 (day06)
import Day07 (day07)
import Day08 (day08)
import Day09 (day09)
import Day10 (day10)
import Day11 (day11)
import Day12 (day12)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["1"] -> day01
        ["2"] -> day02
        ["3"] -> day03
        ["4"] -> day04
        ["5"] -> day05
        ["6"] -> day06
        ["7"] -> day07
        ["8"] -> day08
        ["9"] -> day09
        ["10"] -> day10
        ["11"] -> day11
        ["12"] -> day12
        _ -> do
            putStrLn "Usage: cabal run aoc2020 -- <day>"
            exitWith $ ExitFailure 1
