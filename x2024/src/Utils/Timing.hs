module Utils.Timing(timeMethod) where
import Data.Time ( NominalDiffTime, diffUTCTime )
import Data.Time.Clock (getCurrentTime)

timeMethod :: IO a -> IO (a, NominalDiffTime)
timeMethod method = do
   startTime <- getCurrentTime
   result <- method
   endTime <- getCurrentTime
   let diffTime = diffUTCTime endTime startTime
   return (result, diffTime)
