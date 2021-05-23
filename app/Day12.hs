module Day12 (day12) where

import System.IO
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

day12 :: IO ()
day12 = withFile "data/12.txt" ReadMode \fin -> do
    c <- TIO.hGetContents fin
    let commands = T.lines c

    let s0 = State { x = 0, y = 0, dir = 0 }
    let s = foldl (flip applyCommand) s0 commands
    putStr "part 1: "
    print $ abs (x s) + abs (y s)

    let s0' = State2 { x2 = 0, y2 = 0, wx = 10, wy = 1 }
    let s' = foldl (flip applyCommand2) s0' commands    
    putStr "part 2: "
    print $ abs (x2 s') + abs (y2 s')

data State = State
    { x :: Int
    , y :: Int
    , dir :: Int
    } deriving (Show)

applyCommand :: Text -> State -> State
applyCommand cmd State {..} = let
    c = T.head cmd
    n :: Int = read $ T.unpack $ T.tail cmd
    in case c of
        'N' -> State { y = y + n, .. }
        'S' -> State { y = y - n, .. }
        'E' -> State { x = x + n, .. }
        'W' -> State { x = x - n, .. }
        'L' -> State { dir = dir + n, .. }
        'R' -> State { dir = dir - n, .. }
        'F' -> let
            (dx, dy) = case dir `mod` 360 of
                0 -> (1, 0)
                90 -> (0, 1)
                180 -> (-1, 0)
                270 -> (0, -1)
                _ -> undefined
            in State { x = x + n * dx, y = y + n * dy, dir }
        _ -> undefined

data State2 = State2
    { x2 :: Int
    , y2 :: Int
    , wx :: Int 
    , wy :: Int
    } deriving (Show)

applyCommand2 :: Text -> State2 -> State2
applyCommand2 cmd State2 {..} = let
    c = T.head cmd
    n :: Int = read $ T.unpack $ T.tail cmd
    rotate State2 { .. } angle = case angle `mod` 360 of
        0 -> State2 { .. }
        90 -> State2 { wy = wx, wx = -wy, .. }
        180 -> State2 { wx = -wx, wy = -wy, .. }
        270 -> State2 { wx = wy, wy = -wx, .. }
        _ -> undefined
    in case c of
        'N' -> State2 { wy = wy + n, .. }
        'S' -> State2 { wy = wy - n, .. }
        'E' -> State2 { wx = wx + n, .. }
        'W' -> State2 { wx = wx - n, .. }
        'L' -> rotate State2 { .. } n
        'R' -> rotate State2 { .. } (-n)
        'F' -> State2 { x2 = x2 + n * wx, y2 = y2 + n * wy, .. }
        _ -> undefined
