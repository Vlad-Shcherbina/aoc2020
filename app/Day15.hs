module Day15 (day15) where

import qualified Data.Map.Strict as Map

day15 :: IO ()
day15 = do
    let 
        input = [16,12,1,0,15,7,11]
        step0 m (t, x) = Map.insert x t m
        last_seen = foldl step0 Map.empty $ zip [0..] (init input)
        loop last_seen last t = let
            (old, last_seen') = Map.insertLookupWithKey (\_ a _ -> a) last t last_seen
            x = case old of
                Just tt -> t - tt
                Nothing -> 0
            in x : loop last_seen' x (t + 1)
        chain = input ++ loop last_seen (last input) (length input - 1)
    putStr "part 1: "
    print $ chain !! (2020 - 1)
