module Utils.Math where

distance :: Num a => a -> a -> a
distance x z = abs (x-z) 

isInBound :: (Ord a,Num a) => a -> a -> a -> Bool
isInBound lower upper num = (lower <= num) && (num <= upper)