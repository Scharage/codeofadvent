{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module X7.X7Operator(formatIntoCalibration,generateOperantChoices,addAllChoices,addAllExtChoices,totalCalibrationResult) where
import Utils.InputFormat (readWordsAndLines, toNum)
import Utils.Structures (combinations)

data Operant = Plus | Mul | Concat
instance Show Operant where
  show Plus = "+"
  show Mul = "*"
  show Concat = "||"

type Choices a = [a]

type Calibration = (Int,[Int],Choices [Operant])

operantToBifunction ::  Operant -> (Int -> Int -> Int)
operantToBifunction Plus = (+)
operantToBifunction Mul = (*)
operantToBifunction Concat = concatTwoNums

concatTwoNums ::  Int -> Int -> Int
concatTwoNums a b = toNum $ show a ++ show b

lineToCalibration:: [String] -> Calibration
lineToCalibration (x:xs) = (toNum $ init x,map toNum xs,[])
lineToCalibration [] = (0,[],[])

formatIntoCalibration::String -> [Calibration]
formatIntoCalibration s = map lineToCalibration $ readWordsAndLines s

generateOperantChoices::Int->Choices [Operant]
generateOperantChoices len = combinations len [Plus,Mul]

generateExtOperantChoices::Int->Choices [Operant]
generateExtOperantChoices len = combinations len [Plus,Mul,Concat]

addAllChoices:: [Calibration] -> [Calibration]
addAllChoices cal = map (\(ca,nums,_)->(ca,nums,generateOperantChoices $ length nums - 1)) cal

addAllExtChoices cal = map (\(ca,nums,_)->(ca,nums,generateExtOperantChoices $ length nums - 1)) cal

calcNumsWithOperant':: Int -> [Operant] -> [Int] -> Int
calcNumsWithOperant' prev [] [] = prev
calcNumsWithOperant' prev (op:ops) (n:ns) =  calcNumsWithOperant' (operantToBifunction op prev n ) ops ns
calcNumsWithOperant' prev _ _ = prev

calcNumsWithOperant::[Int] -> [Operant] -> Int
calcNumsWithOperant (n:ns) ops = calcNumsWithOperant' n ops ns
calcNumsWithOperant _ _ = error "well well well"

checkCalibration:: Calibration -> Bool
checkCalibration (_,_,[]) = False
checkCalibration (ca,nums,ops:chois) = calcNumsWithOperant nums ops == ca || checkCalibration (ca,nums,chois)

--totalCalibrationResult:: [Calibration] -> Int
totalCalibrationResult :: [Calibration] -> Int
totalCalibrationResult cals = sum $ map (\(cal,_,_)->cal) $ filter checkCalibration cals