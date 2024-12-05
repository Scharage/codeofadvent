module Utils.Structures (mapTuple, combine , count, skipIndex, permutationsOfSkips, indexList,matrixToAllPossibleRows,diagonals,rowsToColumns,turn90degree,middleOfList) where
import Data.List (transpose)

mapTuple :: (t -> b) -> (t, t) -> (b, b)
mapTuple f (x1,x2)= (f x1, f x2)

combine :: (a -> b -> c) -> (a,b) -> c
combine f (a,b) = f a b

count   :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)

skipIndex :: Int -> [a] -> [a]
skipIndex i xs = z ++ drop 1 zs
    where (z,zs) = splitAt i xs

permutationsOfSkips :: [a] -> [[a]]
permutationsOfSkips xs = xs:[skipIndex i xs |i<-[0..(length xs - 1)]]

indexList :: (Num i, Enum i) => [a] -> [(a, i)]
indexList xs = zip xs [0..]

diagonals :: [[a]] -> [[a]]
diagonals = tail . go [] where
    go b es_ = [h | h:_ <- b] : case es_ of
        []   -> transpose ts
        e:es -> go (e:ts) es
        where ts = [t | _:t <- b]

rowsToColumns:: [[a]]->[[a]]
rowsToColumns  = transpose

turn90degree :: [[a]] -> [[a]]
turn90degree = transpose . map reverse

matrixToAllPossibleRows :: [[a]] -> [[a]]
matrixToAllPossibleRows xs = xs ++ rowsToColumns xs ++ diagonals xs ++ diagonals (turn90degree xs)


middleOfList:: [a] -> Maybe a
middleOfList [x] = Just x
middleOfList [_,x,_] = Just x
middleOfList [_,_] = Nothing
middleOfList (_:rs) = middleOfList (init rs)
middleOfList [] = Nothing