module X6.X6Main (x6main) where
import Utils.InputFormat (readInputIntoMatrixMap)
import X6.X6Pathing (coordsMapToGamePlan, startGame, countDistinctStep, startParadox, countDistinctParadox)


x6main :: IO ()
x6main = do
    input <- readFile "./app/X6/input.txt"
    let gameplan = coordsMapToGamePlan (readInputIntoMatrixMap input)
    let resultplan = startGame gameplan
    let steps = countDistinctStep resultplan
    let paradoxplan = startParadox gameplan
    let paradoxes = countDistinctParadox paradoxplan
    print $ "Distinct Steps: " ++ show steps
    print $ "Distinct Paradoxes: " ++ show paradoxes