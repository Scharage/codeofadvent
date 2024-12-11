{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE ViewPatterns #-}
module X9.X9Disk where
import Utils.InputFormat
import Utils.Structures
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldr


data Space = Taken Int | Free
instance Show Space where
    show (Taken d) = show d
    show Free = "."
type Disk = [Space]
type FastDisk = Seq.Seq Space

isTaken :: Space -> Bool
isTaken (Taken _) = True
isTaken _ = False

isFree :: Space -> Bool
isFree = not.isTaken


diskMapToDisk'free::Int -> [Int]->Disk
diskMapToDisk'free _ [] = []
diskMapToDisk'free _ [0] = []
diskMapToDisk'free did (0:xs) = diskMapToDisk'taken did xs
diskMapToDisk'free _ [s] = [Free| _<-[1..s]]
diskMapToDisk'free did (s:xs) = [Free| _<-[1..s]] ++ diskMapToDisk'taken did xs

diskMapToDisk'taken::Int-> [Int] -> Disk
diskMapToDisk'taken _ [] = []
diskMapToDisk'taken did [s] = [Taken did| _<-[1..s]]
diskMapToDisk'taken did (s:xs) = [Taken did| _<-[1..s]] ++ diskMapToDisk'free (did+1) xs

diskMapToDisk::[Char] -> Disk
diskMapToDisk s = diskMapToDisk'taken 0 $ map toDigit s

splitTaken :: FastDisk -> (FastDisk, FastDisk)
splitTaken = Seq.spanl isTaken

splitFree :: FastDisk -> (FastDisk, FastDisk)
splitFree = Seq.spanr isFree

switchBeginAndEnd :: FastDisk -> FastDisk
switchBeginAndEnd d | Seq.null d = Seq.empty
                    | Seq.length d == 1 = d
                    | otherwise =  e Seq.<| (body Seq.|> h)
                                    where (Seq.viewl -> h Seq.:< tl) = d
                                          (Seq.viewr -> body Seq.:> e) = tl
                                       

sortDisk':: FastDisk -> FastDisk
sortDisk' d | Seq.null d = Seq.empty
            | otherwise = takenD Seq.>< sortDisk' (switchBeginAndEnd unsortedD) Seq.>< freeD
            where (takenD,rest) = splitTaken d
                  (freeD,unsortedD) = splitFree rest

sortDisk::Disk -> Disk
sortDisk [] = []
sortDisk d = Foldr.toList $ sortDisk' $ Seq.fromList d

mulSpaceIndex::(Space,Int)->Int
mulSpaceIndex (Free,_) = 0
mulSpaceIndex (Taken did,i) = i * did

checksum::Disk -> Int
checksum d = sum $ map mulSpaceIndex $ indexList d

