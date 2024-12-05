module Utils.InputFormat (toNum,lineToNums,readNumsInLine,readSections) where
import Data.List.Split (splitOn)

toNum :: String -> Int
toNum x = read x :: Int

lineToNums:: String -> [Int]
lineToNums s = map toNum (words s)

readNumsInLine:: String -> [[Int]]
readNumsInLine s = map lineToNums (lines s)

readSections :: String -> [[String]]
readSections xs = splitOn [""] (lines xs)