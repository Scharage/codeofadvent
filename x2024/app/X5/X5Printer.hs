module X5.X5Printer(parseRule, parsePrintOrder,filterForCorrectPrintOrder,findMiddleOfPrintOrders,filterForInCorrectPrintOrder,fixPrintOrder) where
import Utils.InputFormat (toNum)
import Data.List.Split (splitOn)
import Utils.BoolChecks (isAnyInList)
import Utils.Structures (middleOfList)
import Data.Maybe (fromJust)

type Rule = (Int,Int)
type PrintOrder = [Int]

parseRule::String->Rule
parseRule [d1,d2,'|',d3,d4] = (toNum [d1,d2],toNum [d3,d4])
parseRule _ = (0,0)

parsePrintOrder::String -> [Int]
parsePrintOrder s = map toNum (splitOn "," s)

getYsRuleNotBeforeX::[Rule]-> Int -> [Int]
getYsRuleNotBeforeX rs x = map snd (filter (\r -> fst r == x) rs)


checkCorrectPrintOrder' :: [Rule]->PrintOrder->PrintOrder->Bool
checkCorrectPrintOrder' _ _ [] = True --Nothing left to check so True
checkCorrectPrintOrder' rs [] (x:xs) = checkCorrectPrintOrder' rs [x] xs -- First doesn't need a check
checkCorrectPrintOrder' rs clean (x:xs) | isAnyInList (`elem` clean) (getYsRuleNotBeforeX rs x) = False
                                        | otherwise = checkCorrectPrintOrder' rs (clean ++ [x]) xs

checkCorrectPrintOrder:: [Rule]->PrintOrder->Bool
checkCorrectPrintOrder rs = checkCorrectPrintOrder' rs []

filterForCorrectPrintOrder:: [Rule] -> [PrintOrder] -> [PrintOrder]
filterForCorrectPrintOrder rs = filter (checkCorrectPrintOrder rs)

filterForInCorrectPrintOrder :: [Rule] -> [PrintOrder] -> [PrintOrder]
filterForInCorrectPrintOrder rs = filter (not.checkCorrectPrintOrder rs)

findMiddleOfPrintOrders::[PrintOrder] -> [Int]
findMiddleOfPrintOrders = map (fromJust . middleOfList)

fixPrintOrder':: [Rule] -> PrintOrder -> PrintOrder -> PrintOrder
fixPrintOrder' _ front [] = front --fix done
fixPrintOrder' rs [] (x:end) = fixPrintOrder' rs [x] end
fixPrintOrder' rs front (x:end) | isAnyInList (`elem` front) (getYsRuleNotBeforeX rs x) = fixPrintOrder' rs (init front) (x:l:end) -- rule conflict, time to fix
                                | otherwise = fixPrintOrder' rs (front++[x]) end
                                where l = last front


fixPrintOrder:: [Rule] -> PrintOrder -> PrintOrder
fixPrintOrder rs = fixPrintOrder' rs []