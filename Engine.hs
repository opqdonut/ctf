module Engine where

import qualified Data.Map as M
import Control.Monad
import Data.List
import Control.Monad.RWS
import Data.Array
import Data.Maybe

-- | Geometry

type Coord = (Int,Int)

data Dir = S | D | U | L | R
         deriving (Show,Read,Eq)

type Trans = (Int,Int)

trans :: Dir -> Trans
trans S = (0,0)
trans U = (-1,0)
trans D = (1,0)
trans L = (0,-1)
trans R = (0,1)

manhattan :: Coord -> Coord -> Int
manhattan (a,b) (c,d) = abs (a-c) + abs (b-d)
               
(<+>) :: Coord -> Trans -> Coord
(a,b) <+> (c,d) = (a+c,b+d)

move :: Dir -> Coord -> Coord
move d c = c <+> trans d

-- | Types

data Team = A | B
          deriving (Show, Eq, Ord, Ix)
                   
opposing A = B
opposing B = A

type Points = Array Team Int

addPoints :: Team -> Int -> Points -> Points
addPoints t i p = p // [(t,p!t + i)]

data Flag = Flag {flagTeam :: Team, flagCoord :: Coord}
          deriving (Show)

type Flags = Array Team Flag

fromFlags = elems
toFlags = array (A,B) . map f
  where f flag = (flagTeam flag, flag)

data SoldierState = SoldierState {soldierName :: Name,
                                  soldierTeam :: Team,
                                  soldierCoord :: Coord,
                                  soldierCooldown :: Int,
                                  soldierAlive :: Bool,
                                  holdsFlag :: Maybe Team}
                  deriving Show
                           
mkSoldier :: Name -> Team -> Coord -> Int -> SoldierState
mkSoldier n t c g = SoldierState n t c g True Nothing

type Soldiers = M.Map Name SoldierState

toSoldiers :: [SoldierState] -> Soldiers
toSoldiers = M.fromList . map f
  where f s = (soldierName s, s)
        
fromSoldiers :: Soldiers -> [SoldierState]
fromSoldiers = M.elems

-- traversable?
mapMSoldiers :: Monad m => (SoldierState -> m SoldierState) -> Soldiers -> m Soldiers
mapMSoldiers f s = liftM toSoldiers $ mapM f (fromSoldiers s)

mapMNamedSoldier :: Monad m => (SoldierState -> m SoldierState) -> Name -> Soldiers -> m Soldiers
mapMNamedSoldier f name ss = do s' <- f (ss M.! name)
                                return $ M.insert name s' ss

type Name = String

data Grenade = Grenade {grenadeCoord :: Coord, grenadeTeam :: Team, countdown :: Int}
               deriving Show

data Command = Command {commandName :: Name,
                        commandDirection :: Dir,
                        throwsGrenade :: Maybe Coord}
               deriving (Show,Eq)

-- | Events

data Respawn = Respawn {respawnName :: Name} deriving Show
data Explosion = Explosion Coord deriving Show

data Event = EvGrenade Grenade | EvExplosion Explosion | EvRespawn Respawn
           deriving Show
                    
type EventM = RWS () [Event] Game

runEventM :: EventM () -> Game -> (Game,[Event])
runEventM f g = execRWS f () g

pend :: Event -> EventM ()
pend = tell . (:[])

-- | Rules

data Rules = Rules {nRounds :: Int,
                    pointsKill :: Int,
                    pointsSteal :: Int,
                    pointsCapture :: Int, -- not used yet
                    grenadeRange :: Int,
                    grenadeCooldown :: Int}
           deriving (Read,Show)
             
defaultRules = Rules {nRounds = 100,
                      pointsKill = 1,
                      pointsSteal = 10,
                      pointsCapture = 100,
                      grenadeRange = 10,
                      grenadeCooldown = 8}

-- | Command handling

givePoints :: Team -> Int -> EventM ()
givePoints t i = modify $ \g -> g {points = addPoints t i (points g)}

moveSoldier :: Command -> SoldierState -> EventM SoldierState
moveSoldier c ss = 
  do let to = move (commandDirection c) (soldierCoord ss)
     ok <- gets $ validCoordinate to . board
     return $ if soldierAlive ss && ok
              then ss {soldierCoord = to}
              else ss
                   
getFlag t = gets $ (!t) . flags
putFlag f = modify $ \g -> g {flags = flags g // [(flagTeam f, f)]}

maybePickUpFlag ss =
  do opposingFlag <- getFlag opponent
     if flagCoord opposingFlag == soldierCoord ss && isNothing (holdsFlag ss)
       then gets (pointsSteal.rules) >>= givePoints (soldierTeam ss)
            >> return ss {holdsFlag = Just opponent}
       else return ss
  where opponent = opposing (soldierTeam ss)

maybeMoveFlag ss =
  case holdsFlag ss of
    Nothing -> return ss
    Just t -> do let c = soldierCoord ss
                 f <- getFlag t
                 bc <- getBase $ soldierTeam ss
                 if (flagCoord f == bc)
                   then do gets (pointsCapture . rules) >>= givePoints (opposing t)
                           c' <- getBase t
                           putFlag $ Flag t c'
                           return ss { holdsFlag = Nothing }
                   else do putFlag f { flagCoord = c }
                           return ss

throw :: Command -> SoldierState -> EventM SoldierState
throw (Command _ _ Nothing) st = return st
throw (Command _ _ (Just coord)) st =
  do ok <- canThrow st coord
     if ok
       then do pend . EvGrenade $ Grenade coord (soldierTeam st) 2
               cd <- gets $ grenadeCooldown . rules
               return st {soldierCooldown = cd}
       else return st

canThrow :: SoldierState -> Coord -> EventM Bool
canThrow s c = do valid <- gets $ validCoordinate c . board
                  range <- gets $ grenadeRange . rules
                  return $
                    valid 
                    && soldierCooldown s == 0
                    && manhattan (soldierCoord s) c <= range
                    
coolDown :: SoldierState -> EventM SoldierState
coolDown ss = return ss {soldierCooldown = c}
  where c = max 0 (soldierCooldown ss - 1)
                    
-- XXX this is somewhat ugly:
processSoldier :: Maybe Command -> SoldierState -> EventM SoldierState
processSoldier (Just c) = maybePickUpFlag >=> throw c >=> moveSoldier c >=> maybeMoveFlag >=> coolDown
processSoldier Nothing = coolDown

processCommands :: [Command]
                   -> EventM ()
processCommands cs = do new <- gets soldiers >>= mapMSoldiers f
                        modify (\g -> g {soldiers = new})
  where f ss = processSoldier (find ((==soldierName ss).commandName) cs) ss

-- | Event handling

kills :: SoldierState -> Explosion -> Bool
kills s (Explosion (a,b)) = abs (a-c) <= 1 && abs (b-d) <= 1
  where (c,d) = soldierCoord s

reviveSoldier :: SoldierState -> EventM SoldierState
reviveSoldier s
  | not (soldierAlive s) = do r <- gets $ respawn.board
                              c <- gets $ grenadeCooldown.rules
                              return s { soldierAlive = True,
                                         soldierCoord = r (soldierTeam s), 
                                         soldierCooldown = c, holdsFlag = Nothing }
  | otherwise = return s

processEvents :: EventM ()
processEvents = mapM_ processEvent =<< gets pendingEvents 

processEvent :: Event -> EventM ()
processEvent (EvGrenade g)
  | countdown g == 1  = pend . EvExplosion . Explosion $ grenadeCoord g
  | otherwise         = pend $ EvGrenade g {countdown = countdown g - 1} 
processEvent (EvExplosion e) = do new <- gets soldiers >>= mapMSoldiers f
                                  modify (\g -> g {soldiers = new})
  where f s
          | kills s e = do pend . EvRespawn . Respawn $ soldierName s
                           gets (pointsKill . rules) >>= givePoints (opposing $ soldierTeam s)
                           return s {soldierAlive=False}
          | otherwise = return s
processEvent (EvRespawn (Respawn n)) = do new <- gets soldiers >>= mapMNamedSoldier reviveSoldier n
                                          modify (\g -> g {soldiers = new})
                                          
-- | Board

data Tile = Empty | Obstacle | Spawn Team | Base Team
          deriving (Show, Eq)

drawTile Empty = "."
drawTile Obstacle = "#"
drawTile (Spawn A) = "A"
drawTile (Spawn B) = "B"
drawTile (Base A) = "a"
drawTile (Base B) = "b"

data Board = Board {boardContents :: Array Coord Tile}
             
findBoard :: Board -> (Tile -> Bool) -> Maybe Coord
findBoard b pred = fmap fst . find (pred.snd) . assocs . boardContents $ b
             
respawn :: Board -> Team -> Coord
respawn b t = fromJust $ findBoard b pred
  where pred (Spawn t') = t==t'
        pred _ = False

base :: Board -> Team -> Coord
base b t = fromJust $ findBoard b pred
  where pred (Base t') = t==t'
        pred _ = False
        
getBase :: Team -> EventM Coord
getBase t = gets (flip base t . board)

boardSize b = (w+1,h+1)
  where (w,h) = snd . bounds . boardContents $ b

validCoordinate :: Coord -> Board -> Bool
validCoordinate c b = ok && val /= Obstacle
  where ok = inRange (bounds . boardContents $ b) c
        val = boardContents b ! c

-- | Bring it together

data Game = Game {board :: Board,
                  rules :: Rules,
                  soldiers :: Soldiers,
                  flags :: Flags,
                  points :: Points,
                  pendingEvents :: [Event]}

mkGame :: Board -> Rules -> [Name] -> [Name] -> Game
mkGame b rules anames bnames = Game b rules s fs ps []
  where mks t n = mkSoldier n t (respawn b t) (grenadeCooldown rules)
        s = toSoldiers $ map (mks A) anames ++ map (mks B) bnames
        fs = toFlags $ [Flag A (base b A), Flag B (base b B)]
        ps = array (A,B) [(A,0),(B,0)]

updateGame :: Game -> [Command] -> Game
updateGame g commands = g' {pendingEvents = events'}
  where (g',events') = flip runEventM g $ do processEvents
                                             processCommands commands
                         
grenades :: [Event] -> [Grenade]
grenades = concatMap f
  where f e = case e of EvGrenade g -> [g]
                        _ -> []

explosions :: [Event] -> [Explosion]       
explosions = concatMap f
  where f e = case e of EvExplosion ex -> [ex]
                        _ -> []
        
data DrawMode = DrawBoard | DrawAll

drawGame' :: Game -> M.Map Coord String
drawGame' game = M.fromListWith comb (gs++tss++es++fs)
  where comb x y = x ++ "," ++ y
        gs = map drawGrenade . grenades . pendingEvents $ game
        drawGrenade g = (grenadeCoord g,show $ countdown g)
        es = map drawExplosion . explosions . pendingEvents $ game
        drawExplosion (Explosion c) = (c,"%")
        tss = map drawSoldier . M.assocs . soldiers $ game
        drawSoldier (name,s) = (soldierCoord s,if soldierAlive s then name else "_")
        fs = map drawFlag . fromFlags . flags $ game
        drawFlag (Flag t c) = (c,"?"++show t)
        
drawBoardCoord :: Game -> Coord -> String
drawBoardCoord g c = drawTile . (!c) . boardContents . board $ g
        
drawGame :: Game -> DrawMode -> String
drawGame g dm = unlines $ map (intercalate " " . map d) coords
  where (w,h) = boardSize . board $ g
        coords :: [[Coord]]
        coords = map (\x -> map ((,)x) [0..h-1]) [0..w-1]
        drawn = case dm of DrawBoard -> M.empty
                           DrawAll -> drawGame' g
        d c = M.findWithDefault (drawBoardCoord g c) c drawn

gameInfo :: Game -> Team -> String
gameInfo g t = unlines $ show t:p:fs++ss++gs
  where p = "Points "++show (points g ! t)
        fs = map show . elems $ flags g
        ss = map show . fromSoldiers $ soldiers g
        gs = map show . filter ((==t).grenadeTeam) . grenades $ pendingEvents g
        
gameObjects :: Game -> String
gameObjects g = unlines $ ps++fs++ss
  where ps = map show . assocs $ points g
        fs = map show . fromFlags $ flags g
        ss = map show . fromSoldiers $ soldiers g
        
gamePending :: Game -> String
gamePending g = unlines $ map show $ pendingEvents g
