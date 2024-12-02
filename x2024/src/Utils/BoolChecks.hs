module Utils.BoolChecks where

isListOrd' :: (a -> a -> Bool) -> [a] -> Bool
isListOrd' _ [] = True
isListOrd' _ [x] = True
isListOrd' boolf (x:z:xs) = boolf x z  && isListOrd' boolf (z:xs)

isListOrd :: (a -> a -> Bool) -> [a] -> Bool
isListOrd = isListOrd'

isListDesc:: Ord a => [a] -> Bool
isListDesc = isListOrd (>)

isListAsc:: Ord a => [a] -> Bool
isListAsc = isListOrd (<)