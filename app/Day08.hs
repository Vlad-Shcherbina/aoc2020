module Day08 (day08) where

import System.IO
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Set as S

import Util

day08 :: IO ()
day08 = withFile "data/08.txt" ReadMode \fin -> do
    c <- TIO.hGetContents fin
    let prog = map parseInsn $ T.lines c
    putStr "part 1: "
    print $ part1 prog

part1 :: [Insn] -> Int
part1 prog = loop S.empty 0 0 where
    loop visited ip acc = if ip `S.member` visited
        then acc
        else let
            visited' = S.insert ip visited
            ip' = ip + 1
            in case prog !! ip of
                Nop _ -> loop visited' ip' acc
                Acc x -> loop visited' ip' (acc + x)
                Jmp x -> loop visited' (ip + x) acc

data Insn
    = Nop Int
    | Acc Int
    | Jmp Int
    deriving (Show)

parseInsn :: Text -> Insn
parseInsn s = let
    Just (op, arg) = partition " " s
    in
    case op of
        "acc" -> Acc $ parseInt arg
        "jmp" -> Jmp $ parseInt arg
        "nop" -> Nop $ parseInt arg
        _ -> undefined

parseInt :: Text -> Int
parseInt s = case T.stripPrefix "+" s of
    Just s -> read $ T.unpack s
    Nothing -> read $ T.unpack s
