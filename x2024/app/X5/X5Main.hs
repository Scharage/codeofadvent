module X5.X5Main (x5main) where
import Utils.InputFormat (readSections)
import X5.X5Printer (parseRule, parsePrintOrder, filterForCorrectPrintOrder, findMiddleOfPrintOrders, filterForInCorrectPrintOrder, fixPrintOrder)
import Utils.Timing (timeMethod)

x5main :: IO ()
x5main = do
    (input,inputTime) <- timeMethod (readFile "./app/X5/input.txt")
    let [rawRules,rawPrint] = readSections input
    let rules = map parseRule rawRules
    let printOrder = map parsePrintOrder rawPrint
    (result,diff1) <- timeMethod (return (sum (findMiddleOfPrintOrders (filterForCorrectPrintOrder rules printOrder))))
    (result2,diff2) <- timeMethod (return (sum (findMiddleOfPrintOrders (map (fixPrintOrder rules) (filterForInCorrectPrintOrder rules printOrder)))))
    print ("Input Time: "++show inputTime)
    print ("MiddlePrints: "++show result++" Time: "++ show (diff1/1000))
    print ("Incorrect MiddlePrints: "++show result2++" Time: "++ show diff2)