module X5.X5Main (x5main) where
import Utils.InputFormat (readSections)
import X5.X5Printer (parseRule, parsePrintOrder, filterForCorrectPrintOrder, findMiddleOfPrintOrders, filterForInCorrectPrintOrder, fixPrintOrder)


x5main :: IO ()
x5main = do
    input <- readFile "./app/X5/input.txt"
    let [rawRules,rawPrint] = readSections input
    let rules = map parseRule rawRules
    let printOrder = map parsePrintOrder rawPrint
    let result = sum (findMiddleOfPrintOrders (filterForCorrectPrintOrder rules printOrder))
    let result2 = sum (findMiddleOfPrintOrders (map (fixPrintOrder rules) (filterForInCorrectPrintOrder rules printOrder)))
    print ("MiddlePrints: "++show result)
    print ("Incorrect MiddlePrints: "++show result2)