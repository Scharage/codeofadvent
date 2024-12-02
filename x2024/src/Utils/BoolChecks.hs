module Utils.BoolChecks (isListOrd,isListDesc,isListAsc,isAnyInList) where

isListOrd' :: (a -> a -> Bool) -> [a] -> Bool
isListOrd' _ [] = True
isListOrd' _ [_] = True
isListOrd' boolf (x:z:xs) = boolf x z  && isListOrd' boolf (z:xs)

isListOrd :: (a -> a -> Bool) -> [a] -> Bool
isListOrd = isListOrd'

isListDesc:: Ord a => [a] -> Bool
isListDesc = isListOrd (>)

isListAsc:: Ord a => [a] -> Bool
isListAsc = isListOrd (<)

isAnyInList' :: (t -> Bool) -> [t] -> Bool
isAnyInList' _ []  = False
isAnyInList' boolf (x:xs)  = boolf x || isAnyInList' boolf xs

isAnyInList :: (t -> Bool) -> [t] -> Bool
isAnyInList = isAnyInList'