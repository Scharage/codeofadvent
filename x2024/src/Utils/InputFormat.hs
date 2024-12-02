module Utils.InputFormat (toNum,lineToNums,readNumsInLine) where

toNum :: String -> Int
toNum x = read x :: Int

lineToNums:: String -> [Int]
lineToNums s = map toNum (words s)

readNumsInLine:: String -> [[Int]]
readNumsInLine s = map lineToNums (lines s)