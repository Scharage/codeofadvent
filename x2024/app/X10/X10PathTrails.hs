module X10.X10PathTrails where
import Utils.Coords
import qualified Data.Map.Lazy as Map
import qualified Data.List.NonEmpty
import Data.List

type Tile = (Coords,Int)
type Path = [Tile]
type Trails = [Path]
type Mountains = Map.Map Coords Int

spreadCoords:: Coords -> [Coords]
spreadCoords coords = [up coords,right coords,down coords,left coords]
                        

pathing'::Mountains -> Trails -> Trails
pathing' _ ps@(((Coords _ _,9):_):_) = ps
pathing' ms ps = pathing' ms [(cCoord,h2):p|p@((lead,h):_)<-ps,cCoord<-spreadCoords lead,Map.member cCoord ms,h2<-[ms Map.! cCoord],h+1 == h2]

pathing:: Mountains -> [Trails]
pathing ms = [pathing' ms [[trailhead]] |trailhead@(_,h)<-Map.assocs ms,h==0]

scoreTrailHead::Trails -> Int
scoreTrailHead ps = length $  map Data.List.NonEmpty.head . Data.List.NonEmpty.group . sort $ map head ps
                                 