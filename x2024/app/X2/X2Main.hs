module X2.X2Main where
import Utils.InputFormat (readNumsInLine)
import X2.ReportAnalysis (countSafeReports)


x2main :: IO ()
x2main = do
    input <- readFile "./app/X2/input.txt"
    let reports = readNumsInLine input
    print ("Safe Reports: " ++ show (countSafeReports reports))