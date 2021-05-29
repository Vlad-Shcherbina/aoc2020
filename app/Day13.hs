module Day13 (day13) where

import System.IO
import Data.Function ((&))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

day13 :: IO ()
day13 = withFile "data/13.txt" ReadMode \fin -> do
    c <- TIO.hGetContents fin
    let [t', schedule'] = T.lines c
    let t :: Int = read $ T.unpack t'
    let schedule :: [Maybe Int] = T.splitOn "," schedule' & map \case
            "x" -> Nothing
            s -> Just $ read $ T.unpack s

    let (w, id) = minimum [((-t) `mod` m, m) | Just m <- schedule]
    putStr "part 1: "
    print (w * id)
    putStr "part 2: "
    print $ crt [(m, -r) | (Just m, r) <- zip schedule [0..]]

-- egcd a b = (x, y, g)
-- such that a * x + b * y = g
egcd :: (Integral a) => a -> a -> (a, a, a)
egcd a 0 = (1, 0, a)
egcd a b = let
    (d, m) = a `divMod` b
    (x1, y1, g) = egcd b m
    in (y1, x1 - y1 * d, g)

-- Chinese remainder theorem
-- crt [(m1, r1), ...] = x
-- x % m1 = r1
-- ...
crt :: (Integral a) => [(a, a)] -> a
crt mrs = 
    sum [y * nn * r | (m, r) <- mrs, let nn = n `div` m, let (_, y, 1) = egcd m nn] `mod` n
    where n = product $ map fst mrs
