module Main where
import SortAndDistance
import FormatList


mapTuple :: (t -> b) -> (t, t) -> (b, b)
mapTuple f (x1,x2)= (f x1, f x2)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let (l1,l2) = mapTuple sortAcc (transformList input)
    let distances = distanceList l1 l2
    let similiars = similiar l1 l2
    print ("Distance: " ++ show (sum distances))
    print ("Similiar: " ++ show (sum similiars))