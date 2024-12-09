{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module X8.X8Antenna where
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Utils.Coords
import Utils.Structures (combinations, doSet, isSingleton)

type AntennaPos = (Coords,Char)
type Antenna = Char
type AntennaPlan = Map.Map Coords Antenna
type GroupedByAntenna = Map.Map Antenna [Coords]


filterOutUseless:: AntennaPlan -> AntennaPlan
filterOutUseless = Map.filter (/= '.')

groupMatrixByAntenna':: [Antenna] -> AntennaPlan -> [(Antenna,[Coords])]
groupMatrixByAntenna' [] _ = []
groupMatrixByAntenna' (a:as) antennamap = (a,Map.keys amap):groupMatrixByAntenna' as rmap
                                        where (amap,rmap) = Map.partition (==a) antennamap

groupMatrixByAntenna:: AntennaPlan -> GroupedByAntenna
groupMatrixByAntenna antennamap = Map.fromList $ groupMatrixByAntenna' as antennamap
                                where as = Set.toList $ Set.fromList $ Map.elems antennamap


findAntinodeCoords:: [(Coords,Coords)]-> [Coords]
findAntinodeCoords [] = []
findAntinodeCoords ((orig,next):cs) = orig `anti` next : findAntinodeCoords cs

antennaPairs::[Coords] -> [(Coords,Coords)]
antennaPairs as = map (\x->(head x,last x)) $ filter (\x -> head x /= last x) $ combinations 2 as

matrixToAntinodes:: GroupedByAntenna -> [Coords]
matrixToAntinodes as = doSet . concatMap (findAntinodeCoords . antennaPairs) $ Map.elems as

isOutOfBounds:: Coords -> Coords -> Bool
isOutOfBounds upperBound c = c `smaller` Coords 0 0 || c `bigger` upperBound

dropOutOfBoundsAntinodes::  Coords -> [Coords] -> [Coords]
dropOutOfBoundsAntinodes upperBound = filter (not . isOutOfBounds upperBound)

findHarmonic::Coords -> Coords -> Coords -> [Coords]
findHarmonic upperBound antinode diff = if isOutOfBounds upperBound next then [] else next:findHarmonic upperBound next diff
                            where next = antinode `plus` diff

findHarmonicAntinodeCoords:: Coords -> [(Coords,Coords)]-> [Coords]
findHarmonicAntinodeCoords _ [] = []
findHarmonicAntinodeCoords upperBound ((orig,next):cs) = findHarmonic upperBound orig (orig `minus` next)  ++ findHarmonicAntinodeCoords upperBound cs

getAllPairedAntennaCoords:: GroupedByAntenna -> [Coords]
getAllPairedAntennaCoords as = concat $ Map.elems (Map.filter (not.isSingleton) as)

matrixToHarmonics:: Coords -> GroupedByAntenna -> [Coords]
matrixToHarmonics upperBound as = doSet $ concat (map (findHarmonicAntinodeCoords upperBound . antennaPairs) (Map.elems as)) ++ getAllPairedAntennaCoords as

test as = (map antennaPairs (Map.elems as))

test2 upperBound pairs = map (findHarmonicAntinodeCoords upperBound) pairs

