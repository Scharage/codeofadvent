{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module X11.X11Main where
import Utils.InputFormat (toNum)
import X11.X11Stones (blink, toBlinkedStones, countBlinkedStones, blinkMap, startBlink)


x11main :: IO ()
x11main = do
    input <- readFile "./app/X11/input.txt"
    let stones = map toNum $ words input
    let blinkedStones = startBlink 25 stones
    let bigStones = startBlink 75 stones
    print $ "Amount of Stones: " ++ show  blinkedStones
    print $ "Amount of Many Stones: " ++ show bigStones