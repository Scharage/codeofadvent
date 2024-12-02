module X2.ReportAnalysis where
import Utils.Math (isInBound, distance)
import Utils.BoolChecks (isListAsc, isListDesc,isListOrd)
import Utils.Structures (count)

isInReportBound :: (Ord a,Num a) => a -> Bool
isInReportBound = isInBound 1 3

distanceBetween1And3 :: (Ord a,Num a) => a -> a -> Bool
distanceBetween1And3 x z = isInReportBound (distance x z)

isListDistanceSafe :: (Ord a,Num a) => [a] -> Bool
isListDistanceSafe =  isListOrd distanceBetween1And3

isReportSafe :: (Ord a,Num a) => [a] -> Bool
isReportSafe xs = (isListAsc xs || isListDesc xs) && isListDistanceSafe xs

countSafeReports :: (Ord a,Num a) => [[a]] -> Int
countSafeReports reports = count True (map isReportSafe reports) 