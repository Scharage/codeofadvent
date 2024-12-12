{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module X10.X10Main where
import Utils.InputFormat
import qualified Data.Map.Lazy as Map
import X10.X10PathTrails


x10main :: IO ()
x10main = do
    input <- readFile "./app/X10/input.txt"
    let matrix = Map.map toDigit $ readInputIntoMatrixMap input
    let paths = pathing matrix
    let scores = map scoreTrailHead paths
    let ratings = sum $ map length paths
    print $ "Sum of Trail Scores: " ++ show (sum scores)
    print $ "Sum of Trail Ratings: " ++ show ratings