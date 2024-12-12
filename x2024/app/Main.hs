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
import X8.X8Main (x8main)
import X9.X9Main (x9main)
import X10.X10Main ( x10main )
import X11.X11Main ( x11main )


main :: IO ()
main = do 
    (_,diff) <- timeMethod x11main
    print $ "Took time: " ++ show diff
