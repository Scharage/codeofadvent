module X3.RegexSolver(mulMatches,mulDoMatches,doMultiplication) where

import Text.Regex.TDFA ( (=~), getAllTextMatches )
import Utils.InputFormat (toNum)
import Utils.Structures (mapTuple, combine)




mulRegex :: String
mulRegex = "mul\\([[:digit:]]{1,3},[[:digit:]]{1,3}\\)"

doRegex :: String
doRegex = "do\\(\\)|don't\\(\\)"

mulDoRegex:: String
mulDoRegex = mulRegex ++ "|" ++ doRegex

mulMatches:: String -> [String]
mulMatches s = getAllTextMatches (s =~ mulRegex) :: [String]

mulInstrMatches:: String -> [String]
mulInstrMatches s = getAllTextMatches (s =~ mulDoRegex) :: [String]

mulDoMatches :: String -> [String]
mulDoMatches = removeDonts . mulInstrMatches

removeDonts' :: [String] -> Bool -> [String]
removeDonts' [] _ = []
removeDonts' ("don't()":xs) _  = removeDonts' xs False
removeDonts' ("do()":xs) _  = removeDonts' xs True
removeDonts' (x:xs) True  = x : removeDonts' xs True
removeDonts' (_:xs) False = removeDonts' xs False

removeDonts :: [String] -> [String]
removeDonts xs = removeDonts' xs True

getMulNumbers':: String  -> (String,String) -> Bool -> (Int,Int)
getMulNumbers' [] res _ = mapTuple toNum res
getMulNumbers' (',':ss) res _ = getMulNumbers' ss res False
getMulNumbers' (')':_) res _ = getMulNumbers' [] res False
getMulNumbers' (s:ss) (l,r) False = getMulNumbers' ss (l,r++[s]) False
getMulNumbers' (s:ss) (l,r) True = getMulNumbers' ss (l++[s],r) True


getMulNumbers :: String -> (Int, Int)
getMulNumbers [] = (0,0)
getMulNumbers ('(':ss) = getMulNumbers' ss ("","") True
getMulNumbers (_:ss) = getMulNumbers ss

doMultiplication :: [String] -> Int
doMultiplication xs = sum (map (combine (*).getMulNumbers) xs)

