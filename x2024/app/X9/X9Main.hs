{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module X9.X9Main where
import X9.X9Disk
import qualified X9.X9BlockyDisk as B'


x9main :: IO ()
x9main = do
    input <- readFile "./app/X9/input_debug.txt"
    let disk = diskMapToDisk input
    let sortedDisk = sortDisk disk
    let sortedChecksum = checksum sortedDisk
    let b'disk = B'.diskMapToDisk input
    let b'sortedDisk = B'.sortDisk b'disk
    let b'sortedChecksum = checksum sortedDisk
    print $ "Checksum of the Disk: " ++ show sortedChecksum
    print $ "Checksum of Blocky Disk: " ++ show b'sortedDisk