module X6.X6Main (x6main) where
import Utils.InputFormat (readInputIntoMatrixMap)
import X6.X6Pathing (coordsMapToGamePlan, startGame, countDistinctStep, startParadox)


x6main :: IO ()
x6main = do
    input <- readFile "./app/X6/input.txt"
    let gameplan = coordsMapToGamePlan (readInputIntoMatrixMap input)
    let resultplan = startGame gameplan
    let steps = countDistinctStep resultplan
    let paradoxes = startParadox gameplan -- Should be 1309? But not solved how to get to it yet
    print $ "Distinct Steps: " ++ show steps
    print $ "Distinct Paradoxes: " ++ show paradoxes