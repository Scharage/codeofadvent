{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main (main) where

import X1.X1Main ( x1main )
import X2.X2Main ( x2main )
import X3.X3Main ( x3main )
import X4.X4Main (x4main)
import X5.X5Main (x5main)
import X6.X6Main (x6main)
import X7.X7Main (x7main)
import Utils.Timing (timeMethod)


main :: IO ()
main = do 
    (_,diff) <- timeMethod x7main
    print $ "Took time: " ++ show diff
