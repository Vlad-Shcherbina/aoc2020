module Day14 (day14) where

import System.IO
import Control.Exception (assert)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import Data.Int ( Int64 )
import Data.List ( foldl', unfoldr )
import Data.Bits
import Util ( partition )

day14 :: IO ()
day14 = withFile "data/14.txt" ReadMode \fin -> do
    c <- TIO.hGetContents fin
    let lines = T.lines c
    let insns = map parse lines
    
    let solve applyInsn = let
            initState = State { mask = Mask { ones = 0, zeros = 0 }, mem = Map.empty}
            State { mem } = foldl' applyInsn initState insns
            in sum $ Map.elems mem
    
    putStr "part 1: "
    print $ solve applyInsn
    putStr "part 2: "
    print $ solve applyInsn2

data State = State { mask :: Mask, mem :: Map.Map Int64 Int64 } deriving (Show)

applyInsn :: State -> Insn -> State
applyInsn state (SetMask newMask) = state { mask = newMask }
applyInsn State { .. } WriteMem { .. } = State {
    mask,
    mem = Map.insert addr (applyMask mask value) mem
    }

applyInsn2 :: State -> Insn -> State
applyInsn2 state (SetMask newMask) = state { mask = newMask }
applyInsn2 State { .. } WriteMem { .. } = State {
    mask,
    mem = foldl' f mem (floatMask (combineMask mask addr))
    } where
        f mem addr = Map.insert addr value mem

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

combineMask :: Mask -> Int64 -> Mask
combineMask Mask {..} addr = Mask {
    ones = ones .|. addr .&. complement zeros,
    zeros = zeros .|. addr
}

floatMask :: Mask -> [Int64]
floatMask Mask {..} = map (ones .|.) (bitPowerset (zeros .&. complement ones))

data Insn
    = SetMask Mask
    | WriteMem { addr :: Int64, value :: Int64 }
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

-- https://www.chessprogramming.org/Traversing_Subsets_of_a_Set#All_Subsets_of_any_Set
bitPowerset :: Int64 -> [Int64]
bitPowerset mask = loop 0 where
    loop x = x : (if y == 0 then [] else loop y) where
        y = (x - mask) .&. mask
