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
