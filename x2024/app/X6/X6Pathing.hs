module X6.X6Pathing(coordsMapToGamePlan,startGame,countDistinctStep,startParadox) where
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Prelude hiding (Left,Right)
import Utils.Coords

data Player = UPlayer | DPlayer | LPlayer | RPlayer deriving (Show,Eq)
data Direction = Up | Down | Left | Right deriving (Show,Eq)
data GameObject = Outside | Obstacle | Step [Direction] | PlayerObject Player | Free  deriving (Show,Eq)



type GamePlan = Map.Map Coords GameObject

toGameObject::Char->GameObject
toGameObject '^' = PlayerObject UPlayer
toGameObject '<' = PlayerObject LPlayer
toGameObject 'v' = PlayerObject DPlayer
toGameObject '>' = PlayerObject RPlayer
toGameObject '#' = Obstacle
toGameObject 'X' = Step []
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
                                          Outside -> Map.insert playerpos (Step []) game -- game ends, guard is out of map
                                          Free -> runGame (Map.insert playerpos (Step []) game) (aheadPos,playerface)
                                          Step _ -> runGame (Map.insert playerpos (Step []) game) (aheadPos,playerface)
                                          PlayerObject _ -> runGame (Map.insert playerpos (Step []) game) (aheadPos,playerface)
                                          Obstacle -> runGame game (playerpos,newPlayerface)
                                                    where newPlayerface = turnPlayerRight playerface
                                          _ -> game
                                        where (aheadPos,ahead) = lookAhead game player

startGame::GamePlan -> GamePlan
startGame game = runGame game (findPlayer game)

countDistinctStep:: GamePlan -> Int
countDistinctStep game = Map.size $ Map.filter (== Step []) game

isPlayerFacingStep :: [Direction] -> Player -> Bool
isPlayerFacingStep dirs UPlayer = Up `elem` dirs
isPlayerFacingStep dirs DPlayer = Down `elem` dirs
isPlayerFacingStep dirs LPlayer = Left `elem` dirs
isPlayerFacingStep dirs RPlayer = Right `elem` dirs


isLooping' :: Map.Map Coords GameObject -> (Coords, Player) -> Int -> Bool
isLooping' _ _ 0 = False
isLooping' game player@(playerpos,playerface) steps = case ahead of
                                          Outside -> False
                                          Free -> isLooping' game (aheadPos,playerface) (steps-1)
                                          Step dirs -> if isPlayerFacingStep dirs playerface then True else isLooping' game (aheadPos,playerface) (steps-1)
                                          PlayerObject _ -> isLooping' game (aheadPos,playerface) (steps-1)
                                          Obstacle ->isLooping' game (playerpos,newPlayerface) (steps-1)
                                                    where newPlayerface = turnPlayerRight playerface
                                        where (aheadPos,ahead) = lookAhead game player

isLooping:: GamePlan -> (Coords,Player) -> Bool
isLooping game player= isLooping' game player 10000


stepFromPlayer :: Player -> GameObject
stepFromPlayer UPlayer = Step [Up]
stepFromPlayer DPlayer = Step [Down]
stepFromPlayer LPlayer = Step [Left]
stepFromPlayer RPlayer = Step [Right]

mergeSteps :: GameObject -> GameObject -> GameObject
mergeSteps (Step xs) (Step ys) = Step (xs ++ ys)
mergeSteps (Step xs) _ = Step xs
mergeSteps _ _ = Step []

runParadox::GamePlan -> (Coords,Player) -> Set.Set Coords
runParadox game player@(playerpos,playerface) = case ahead of
                                          Outside ->  Set.fromList [] -- game ends, guard is out of map
                                          Free -> Set.fromList (if isLooping (Map.insert aheadPos Obstacle game) player  then [aheadPos]  else []) `Set.union` runParadox (Map.adjust (mergeSteps (stepFromPlayer playerface)) playerpos game) (aheadPos,playerface)
                                          Step _ -> Set.fromList (if isLooping (Map.insert aheadPos Obstacle game) player  then [aheadPos]  else []) `Set.union` runParadox (Map.adjust (mergeSteps (stepFromPlayer playerface)) playerpos game) (aheadPos,playerface)
                                          PlayerObject _ -> runParadox (Map.adjust (mergeSteps (stepFromPlayer playerface)) playerpos game) (aheadPos,playerface)
                                          Obstacle -> runParadox (Map.adjust (mergeSteps (stepFromPlayer playerface)) playerpos game) (playerpos,newPlayerface)
                                                    where newPlayerface = turnPlayerRight playerface
                                        where (aheadPos,ahead) = lookAhead game player

startParadox :: GamePlan -> Int
startParadox game = Set.size (runParadox game (findPlayer game))

