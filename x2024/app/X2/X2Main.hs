module X2.X2Main (x2main) where
import Utils.InputFormat (readNumsInLine)
import X2.ReportAnalysis (countSafeReports,countSafeReportsDampener)


x2main :: IO ()
x2main = do
    input <- readFile "./app/X2/input.txt"
    let reports = readNumsInLine input
    print ("Safe Reports: " ++ show (countSafeReports reports))
    print ("Safe Reports: " ++ show (countSafeReportsDampener reports))