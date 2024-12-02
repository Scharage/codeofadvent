module Utils.Structures where

mapTuple :: (t -> b) -> (t, t) -> (b, b)
mapTuple f (x1,x2)= (f x1, f x2)

count   :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)