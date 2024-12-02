module Utils.Structures (mapTuple, count, skipIndex, permutationsOfSkips) where

mapTuple :: (t -> b) -> (t, t) -> (b, b)
mapTuple f (x1,x2)= (f x1, f x2)

count   :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)

skipIndex :: Int -> [a] -> [a]
skipIndex i xs = z ++ drop 1 zs
    where (z,zs) = splitAt i xs

permutationsOfSkips :: [a] -> [[a]]
permutationsOfSkips xs = xs:[skipIndex i xs |i<-[0..(length xs - 1)]]