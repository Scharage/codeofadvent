{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module X11.X11Stones where
import Utils.InputFormat (toNum)
import qualified Data.IntMap as IM
import qualified Data.IntMap as IM

type Stone = Int

hasEvenDigits::Stone->Bool
hasEvenDigits stone = even $ length $ show stone

splitInHalf::Stone->[Stone]
splitInHalf stone = [toNum l,toNum r]
                    where stoneWord = show stone
                          (l,r) = splitAt (length stoneWord `div` 2)  stoneWord

splitInHalfT::Stone->(Stone,Stone)
splitInHalfT stone = (toNum l,toNum r)
                    where stoneWord = show stone
                          (l,r) =  splitAt (length stoneWord `div` 2)  stoneWord

applyRule::Stone->[Stone]
applyRule stone | stone == 0 = [1]
                | hasEvenDigits stone = splitInHalf stone
                | otherwise = [stone*2024]


blink::Int->[Stone]->[Stone]
blink 0 stones = stones
blink n stones = blink (n-1) $ concat [applyRule stone |stone<-stones]
-----------------------
type BlinkedStone = (Stone,Int)

toBlinkedStones::Int->[Stone]->[BlinkedStone]
toBlinkedStones blinks stones = [(stone,blinks)|stone<-stones]

exhaustStone::BlinkedStone -> [BlinkedStone]
exhaustStone (_,0) = []
exhaustStone (stone,blinks) = toBlinkedStones blinksLeft rest ++ exhaustStone (next,blinksLeft)
                        where (next:rest) = applyRule stone
                              blinksLeft = blinks-1

countBlinkedStones::[BlinkedStone] -> Int
countBlinkedStones [] = 0
countBlinkedStones (stone:toDo) = 1 + countBlinkedStones (exhaustStone stone ++ toDo)

------------------------
type Depth = Int
type Amount = Int
type TargetDepth = Int
type DepthMap = IM.IntMap Amount
type StoneMap = IM.IntMap DepthMap

blinkMap::TargetDepth-> Stone-> Depth-> StoneMap -> (Amount,StoneMap)
blinkMap t st d m = case IM.lookup st m of
                        Nothing -> addBlinkToMap t st d m
                        Just dM -> case IM.lookup d dM of
                                        Just a -> (a,m)
                                        Nothing -> addBlinkToMap t st d m

addAmountToMap::StoneMap->Stone->Depth->Amount->StoneMap
addAmountToMap m st d a = case IM.lookup st m of
                            Nothing -> IM.insert st (IM.singleton d a) m
                            Just dM -> IM.insert st (IM.insert d a dM) m

addBlinkToMap::TargetDepth-> Stone-> Depth-> StoneMap -> (Amount,StoneMap)
addBlinkToMap t st d m | t == d = (1,addAmountToMap m st d 1)
                       | st == 0 = (a1,addAmountToMap m1 st d a1) -- Rule 1
                       | hasEvenDigits st = (al2+ar2,addAmountToMap mr2 st d (al2+ar2))
                       | otherwise = (a3,addAmountToMap m3 st d a3)
                       where nextD = d + 1
                             (a1,m1) = blinkMap t 1 nextD m
                             (l2,r2) = splitInHalfT st
                             (al2,ml2) = blinkMap t l2 nextD m
                             (ar2,mr2) = blinkMap t r2 nextD ml2
                             (a3,m3) = blinkMap t (st*2024) nextD m

startBlink'::TargetDepth->StoneMap->[Stone]->Amount
startBlink' _ _ [] = 0
startBlink' t m (st:sts) = a + startBlink' t mm sts
                        where (a,mm) = blinkMap t st 0 m

startBlink :: TargetDepth -> [Stone] -> Amount
startBlink t = startBlink' t IM.empty

