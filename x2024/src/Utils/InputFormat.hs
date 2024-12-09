module Utils.InputFormat (toNum,lineToNums,readNumsInLine,readSections,readInputIntoMatrixMap,readWordsAndLines) where
import Data.List.Split (splitOn)
import Utils.Structures (indexList)
import qualified Data.Map.Lazy as Map
import Utils.Coords


readWordsAndLines:: String -> [[String]]
readWordsAndLines s = map words $ lines s

toNum :: String -> Int
toNum x = read x :: Int

lineToNums:: String -> [Int]
lineToNums s = map toNum (words s)

readNumsInLine:: String -> [[Int]]
readNumsInLine s = map lineToNums (lines s)

readSections :: String -> [[String]]
readSections xs = splitOn [""] (lines xs)

indexIndexToCoordsList :: [([(a, Int)], Int)] -> [(Coords, a)]
indexIndexToCoordsList ass = ass >>= (\(as,y)->map (\(a,x)->(Coords x y,a)) as)

readInputIntoMatrixMap :: String -> Map.Map Coords Char
readInputIntoMatrixMap s = Map.fromList (indexIndexToCoordsList (indexList (map indexList (lines s))))