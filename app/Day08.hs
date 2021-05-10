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

    let Loop part1 = run prog
    putStr "part 1: "
    print part1

    let [part2] = [x | Finished x <- map run $ variations prog]
    putStr "part 2: "
    print part2

variations :: [Insn] -> [[Insn]]
variations prog = do
    (i, insn) <- zip [0..] prog
    alternative <- case insn of
        Nop x -> [Jmp x]
        Jmp x -> [Nop x]
        Acc _ -> []
    return $ take i prog ++ [alternative] ++ drop (i + 1) prog

data RunResult = Loop Int | Finished Int deriving (Show)

run :: [Insn] -> RunResult
run prog = rec S.empty 0 0 where
    rec visited ip acc = if ip `S.member` visited
        then Loop acc
        else let
            visited' = S.insert ip visited
            ip' = ip + 1
            in if ip == length prog
                then Finished acc
                else case prog !! ip of
                    Nop _ -> rec visited' ip' acc
                    Acc x -> rec visited' ip' (acc + x)
                    Jmp x -> rec visited' (ip + x) acc

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
