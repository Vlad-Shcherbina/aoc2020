module Day14 (day14) where

import System.IO
import Control.Exception (assert)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import Data.Int ( Int64 )
import Data.List ( foldl' )
import Data.Bits
import Util ( partition )

day14 :: IO ()
day14 = withFile "data/14.txt" ReadMode \fin -> do
    c <- TIO.hGetContents fin
    let lines = T.lines c
    let insns = map parse lines
    
    let initState = State { mask = Mask { ones = 0, zeros = 0 }, mem = Map.empty}
    let State { mem } = foldl' applyInsn initState insns
    
    putStr "part 1: "
    print $ sum $ Map.elems mem

data State = State { mask :: Mask, mem :: Map.Map Int Int64 } deriving (Show)
applyInsn :: State -> Insn -> State
applyInsn state (SetMask newMask) = state { mask = newMask }
applyInsn State { .. } WriteMem { .. } = State {
    mask,
    mem = Map.insert addr (applyMask mask value) mem
    }

data Mask = Mask { ones :: Int64, zeros :: Int64 } deriving (Show)

parseMask :: T.Text -> Mask
parseMask s = assert (T.length s == 36) $
    let
        ones = T.replace "X" "0" s
        zeros = T.replace "X" "1" s
    in
        Mask { ones = parseBinary ones, zeros = parseBinary zeros }

applyMask :: Mask -> Int64 -> Int64
applyMask Mask {..} x = x .&. zeros .|. ones

data Insn
    = SetMask Mask
    | WriteMem { addr :: Int, value :: Int64 }
    deriving (Show)

parse :: T.Text -> Insn
parse line = case line of
    (T.stripPrefix "mask = " -> Just s) -> SetMask $ parseMask s
    (T.stripPrefix "mem[" -> Just s) ->
        let
            Just (addr, value) = partition "] = " s
        in
            WriteMem { addr = read $ T.unpack addr, value = read $ T.unpack value}
    _ -> undefined

parseBinary :: T.Text -> Int64
parseBinary = T.foldl' op 0 where
    op acc ch = 2 * acc + case ch of
        '0' -> 0
        '1' -> 1
        _ -> undefined
