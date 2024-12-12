module Utils.Coords where

data Coords = Coords {xcoord::Int,ycoord::Int} deriving (Show,Eq)
instance Ord Coords where
    compare x y = compare (coordsToTuple x) (coordsToTuple y)

minus::Coords -> Coords -> Coords
minus (Coords x1 y1) (Coords x2 y2) = Coords (x1 - x2) (y1 - y2)

plus::Coords -> Coords -> Coords
plus (Coords x1 y1) (Coords x2 y2) = Coords (x1 + x2) (y1 + y2)

anti::Coords -> Coords -> Coords
anti orig pos = orig `plus` (orig `minus` pos)

smaller:: Coords -> Coords -> Bool
smaller (Coords x1 y1) (Coords x2 y2) = x1 < x2 || y1 < y2

bigger:: Coords -> Coords -> Bool
bigger (Coords x1 y1) (Coords x2 y2) = x1 > x2 || y1 > y2

coordsToTuple::Coords -> (Int,Int)
coordsToTuple (Coords x y) = (x,y)


isNegative::Coords -> Bool
isNegative (Coords x y) = x < 0 || y < 0

up::Coords->Coords
up (Coords x y) = Coords x (y-1)

down::Coords->Coords
down (Coords x y) = Coords x (y+1)

left::Coords->Coords
left (Coords x y) = Coords (x-1) y

right::Coords->Coords
right (Coords x y) = Coords (x+1) y