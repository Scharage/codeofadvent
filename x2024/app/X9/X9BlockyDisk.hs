{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE ViewPatterns #-}
module X9.X9BlockyDisk where
import qualified Data.Sequence as Seq
import Utils.InputFormat
import qualified Data.Foldable as Foldr

data Space = Taken Int Int | Free Int deriving (Eq)
instance Show Space where
    show ( Taken d n) = show $ concat $ replicate n [d]
    show (Free n) = replicate n '.'
type Disk = [Space]
type FastDisk = Seq.Seq Space

isTaken :: Space -> Bool
isTaken (Taken _ _) = True
isTaken _ = False

isFree :: Space -> Bool
isFree = not.isTaken

diskMapToDisk'free::Int -> [Int] ->Disk
diskMapToDisk'free _ [] = []
diskMapToDisk'free _ [0] = []
diskMapToDisk'free did (0:xs) = diskMapToDisk'taken did xs
diskMapToDisk'free _ [s] = [Free s]
diskMapToDisk'free did (s:xs) = Free s: diskMapToDisk'taken did xs

diskMapToDisk'taken::Int-> [Int] -> Disk
diskMapToDisk'taken _ [] = []
diskMapToDisk'taken did [s] = [Taken did s]
diskMapToDisk'taken did (s:xs) = Taken did s : diskMapToDisk'free (did+1) xs

diskMapToDisk::[Char] -> Disk
diskMapToDisk s = diskMapToDisk'taken 0 $ map toDigit s

splitFree :: FastDisk -> (FastDisk, FastDisk)
splitFree = Seq.spanr isFree

splitLastBlock :: FastDisk -> (FastDisk, FastDisk)
splitLastBlock d = spl d
                where spl Seq.Empty = (Seq.empty,d)
                      spl (Seq.viewr -> body Seq.:> e) = (Seq.singleton e,body)
                      spl _ = (Seq.empty,d)


takenToFree::Space->Space
takenToFree (Taken _ n) = Free n
takenToFree s = s

diskTakenToFree::FastDisk->Space
diskTakenToFree d = spl d
                where spl (Seq.viewl -> h Seq.:< _) = takenToFree h
                      spl _ = Free 0

fitsInto :: Space -> Space -> Bool
fitsInto (Free x) (Free y) = x >= y
fitsInto (Free x) (Taken _ y) = x >= y
fitsInto _ _ = False

takeSingleton::FastDisk -> Space
takeSingleton (Seq.viewl -> h Seq.:< _) = h
takeSingleton _ = Free 0

takeHead::FastDisk -> (FastDisk,FastDisk)
takeHead (Seq.viewl -> h Seq.:< b) = (Seq.singleton h,b)
takeHead b = (Seq.empty,b)

splitNextFreeBlock:: FastDisk -> FastDisk -> (FastDisk,FastDisk,FastDisk)
splitNextFreeBlock blockToFit spaceToLook = (freeSpace,before,after)
                                            where matchingFree = diskTakenToFree blockToFit
                                                  (before,rest) = Seq.breakl (\x -> fitsInto x (takeSingleton blockToFit)) spaceToLook
                                                  (freeSpace,after) = takeHead rest

diffSpace::FastDisk -> FastDisk -> FastDisk
diffSpace (Seq.viewl -> (Free x) Seq.:< _) (Seq.viewl -> (Taken _ y) Seq.:< _)| diff <= 0 = Seq.empty
                                                                              | otherwise = Seq.singleton (Free diff)
                                                                              where diff = x-y
diffSpace _ _ = Seq.empty

sortDisk':: FastDisk -> FastDisk
sortDisk' d | Seq.null d = Seq.empty
            | lastBlock == Seq.empty = Seq.empty
            | freeBlock == Seq.empty = sortDisk' r2  Seq.>< lastBlock Seq.>< freeD
            | otherwise = sortDisk' (before Seq.>< lastBlock Seq.>< spaceLeft Seq.>< after) Seq.>< spaceTaken Seq.>< freeD
            where
                  (freeD,r) = splitFree d
                  (lastBlock,r2) = splitLastBlock r
                  (freeBlock,before,after) = splitNextFreeBlock lastBlock r2
                  spaceLeft = diffSpace freeBlock lastBlock
                  spaceTaken = Seq.singleton (diskTakenToFree lastBlock)

sortDisk::Disk -> Disk
sortDisk [] = []
sortDisk d = Foldr.toList $ sortDisk' $ Seq.fromList d

mulSpaceIndex::(Space,Int)->Int
mulSpaceIndex (Free _,_) = 0
mulSpaceIndex (Taken did _,i) = i * did

breakIntoSingleSpace::Space -> FastDisk
breakIntoSingleSpace (Free n) = Seq.fromList$ replicate n (Free 1)
breakIntoSingleSpace (Taken i n) = Seq.fromList$ replicate n (Taken i 1)

checksum::FastDisk -> Int
checksum d = sum (mulSpaceIndex <$> Seq.mapWithIndex (\i x-> (x,i)) (d >>= breakIntoSingleSpace))