module Main (main) where

import X1.X1Main ( x1main )
import X2.X2Main ( x2main )
import X3.X3Main ( x3main )
import X4.X4Main (x4main)
import X5.X5Main (x5main)
import X6.X6Main (x6main)
import Utils.Timing (timeMethod)


main :: IO ()
--main = x1main
--main = x2main
--main = x3main
--main = x4main
--main = x5main
main = do 
    (_,diff) <- timeMethod x6main
    print $ "Took time: " ++ show diff
