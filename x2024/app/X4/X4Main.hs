module X4.X4Main (x4main) where
import X4.XMasSearcher (countAllXmas, countAllCrossMas)
import Utils.Structures (matrixToAllPossibleRows)


x4main :: IO ()
x4main = do
    input <- readFile "./app/X4/input.txt"
    let matrix = lines input
    let xmas = matrixToAllPossibleRows matrix
    print ("XMAS Count: " ++ show (countAllXmas xmas))
    print ("CrossMas Count: "++ show (countAllCrossMas matrix))