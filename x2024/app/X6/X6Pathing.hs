module X6.X6Pathing(coordsMapToGamePlan,startGame,countDistinctStep) where
import qualified Data.Map.Lazy as Map
import Utils.Structures (Coords(Coords))

data Player = UPlayer | DPlayer | LPlayer | RPlayer deriving (Show,Eq)
data Direction = Up | Down | Left | Right
data StepDir = StepDir [Direction]
data GameObject = Outside | Obstacle | Step | PlayerObject Player | Free  deriving (Show,Eq)



type GamePlan = Map.Map Coords GameObject

toGameObject::Char->GameObject
toGameObject '^' = PlayerObject UPlayer
toGameObject '<' = PlayerObject LPlayer
toGameObject 'v' = PlayerObject DPlayer
toGameObject '>' = PlayerObject RPlayer
toGameObject '#' = Obstacle 
toGameObject 'X' = Step
toGameObject _ = Free

isPlayer:: GameObject -> Bool
isPlayer (PlayerObject UPlayer) = True
isPlayer (PlayerObject DPlayer) = True
isPlayer (PlayerObject LPlayer) = True
isPlayer (PlayerObject RPlayer) = True
isPlayer _ = False

turnPlayerRight:: Player -> Player
turnPlayerRight UPlayer = RPlayer
turnPlayerRight RPlayer = DPlayer
turnPlayerRight DPlayer = LPlayer
turnPlayerRight LPlayer = UPlayer

coordsMapToGamePlan:: Map.Map Coords Char -> GamePlan
coordsMapToGamePlan = Map.map toGameObject

findPlayer:: GamePlan -> (Coords,Player)
findPlayer game = case result of
                  (coord,PlayerObject pl) -> (coord,pl)
                  _ -> error "Player couldn't be found, how the fuq?"

    where
        result =  head (Map.toList (Map.filter isPlayer game))

lookAhead:: GamePlan -> (Coords,Player) -> (Coords,GameObject)
lookAhead game (Coords x y,UPlayer) = (Coords x (y-1),Map.findWithDefault Outside (Coords x (y-1)) game)
lookAhead game (Coords x y,DPlayer) = (Coords x (y+1),Map.findWithDefault Outside (Coords x (y+1)) game)
lookAhead game (Coords x y,LPlayer) = (Coords (x-1) y,Map.findWithDefault Outside (Coords (x-1) y) game)
lookAhead game (Coords x y,RPlayer) = (Coords (x+1) y,Map.findWithDefault Outside (Coords (x+1) y) game)

runGame::GamePlan -> (Coords,Player) -> GamePlan
runGame game player@(playerpos,playerface) = case ahead of
                                          Outside -> Map.insert playerpos Step game -- game ends, guard is out of map
                                          Free -> runGame (Map.insert playerpos Step game) (aheadPos,playerface)
                                          Step -> runGame (Map.insert playerpos Step game) (aheadPos,playerface)
                                          PlayerObject _ -> runGame (Map.insert playerpos Step game) (aheadPos,playerface)
                                          Obstacle -> runGame game (playerpos,newPlayerface)
                                                    where newPlayerface = turnPlayerRight playerface
                                        where (aheadPos,ahead) = lookAhead game player

startGame::GamePlan -> GamePlan
startGame game = runGame game (findPlayer game)

countDistinctStep:: GamePlan -> Int
countDistinctStep game = Map.size $ Map.filter (== Step) game

