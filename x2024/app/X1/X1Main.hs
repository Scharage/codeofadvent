module X1.X1Main where
import X1.SortAndDistance ( distanceList, similiar, sortAcc )
import X1.FormatList ( transformList )
import Utils.Structures (mapTuple)

x1main :: IO ()
x1main = do
    input <- readFile "./app/X1/input.txt"
    let (l1,l2) = mapTuple sortAcc (transformList input)
    let distances = distanceList l1 l2
    let similiars = similiar l1 l2
    print ("Distance: " ++ show (sum distances))
    print ("Similiar: " ++ show (sum similiars))