module X8.X8Main (x8main) where


x8main :: IO ()
x8main = do
    input <- readFile "./app/X8/input_debug.txt"
    print $ input