module X8.X8Main (x8main) where
import Utils.InputFormat (readInputIntoMatrixMap)
import X8.X8Antenna
import qualified Data.Map as Map


x8main :: IO ()
x8main = do
    input <- readFile "./app/X8/input.txt"
    let origMap = readInputIntoMatrixMap input
    let matrixmap = filterOutUseless origMap
    let biggestCoord = last $ Map.keys origMap
    let groupedAntenna = groupMatrixByAntenna matrixmap
    let antinodes = dropOutOfBoundsAntinodes biggestCoord $ matrixToAntinodes groupedAntenna
    let harmonics =  dropOutOfBoundsAntinodes biggestCoord $ matrixToHarmonics biggestCoord groupedAntenna
    print $ "Total distinct antinodes: " ++  show (length antinodes)
    print $ "Total distinct harmonics: " ++  show (length harmonics)