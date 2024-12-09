module X7.X7Main (x7main) where
import X7.X7Operator (formatIntoCalibration, addAllChoices,totalCalibrationResult, addAllExtChoices)


x7main :: IO ()
x7main = do
    input <- readFile "./app/X7/input.txt"
    let emptyCalibrations = formatIntoCalibration input
    let calibrations = addAllChoices emptyCalibrations
    let totalCalibrations = totalCalibrationResult calibrations
    let extCalibrations = addAllExtChoices emptyCalibrations
    let totalExtCalibrations =  totalCalibrationResult extCalibrations
    print $ "Total Calibrations: " ++ show totalCalibrations
    print $ "Toal Ext Calibrations: " ++ show totalExtCalibrations