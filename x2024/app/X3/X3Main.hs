module X3.X3Main (x3main) where
import X3.RegexSolver (mulMatches, mulDoMatches, doMultiplication)


x3main :: IO ()
x3main = do
    input <- readFile "./app/X3/input.txt"
    let muls = mulMatches input
    let mulDos = mulDoMatches input
    print ("Mults: " ++ show  (doMultiplication muls))
    print ("DoMults: " ++  show (doMultiplication mulDos))