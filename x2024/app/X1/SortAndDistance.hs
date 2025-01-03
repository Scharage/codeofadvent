module X1.SortAndDistance ( distanceList, similiar, sortAcc ) where

import Data.List (sortOn)
import Data.Ord (Down(Down))
import Utils.Math(distance)

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortOn Down

sortAcc :: Ord a => [a] -> [a]
sortAcc xs = reverse (sortDesc xs)

distanceList :: Num a => [a] -> [a] -> [a]
distanceList xs zs = [distance x z|(x,z)<-zip xs zs]

count   :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)

similiar :: [Int] -> [Int] -> [Int]
similiar xs zs = map (\y -> y * count y zs) xs