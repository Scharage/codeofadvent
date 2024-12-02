module X1.FormatList where

splitEachSecond :: [(Int,Int)]->([Int] ,[Int])
splitEachSecond xs = ([n | (n,i) <- xs, even i], [n | (n,i) <- xs, odd i])

index :: [Int] -> [(Int,Int)]
index xs = zip xs [0..]

toNum :: String -> Int
toNum x = read x :: Int

transformList:: String -> ([Int] ,[Int])
transformList s = splitEachSecond (index (map toNum (words s)))



