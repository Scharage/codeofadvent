module SortAndDistance where

import Data.List (sortOn)
import Data.Ord (Down(Down))
sortDesc :: Ord a => [a] -> [a]
sortDesc = sortOn Down

sortAcc :: Ord a => [a] -> [a]
sortAcc xs = reverse (sortDesc xs)


distance :: Num a => a -> a -> a
distance x z = abs (x - z)

distanceList :: Num a => [a] -> [a] -> [a]
distanceList xs zs = [distance x z|(x,z)<-zip xs zs]

count   :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)

similiar xs zs = map (\y -> y * count y zs) xs