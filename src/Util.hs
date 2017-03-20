module Util where

clamp :: (Ord a) => a -> a -> a -> a
clamp min max x | x < min   = min
                | x > max   = max
                | otherwise = x
