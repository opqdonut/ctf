module Engine where

import qualified Data.Map as M
import Control.Monad
import Control.Applicative
import Data.List
import Data.Array
import Data.Maybe
import Control.Monad.RWS

type Coord = (Int,Int)

data SoldierState = SoldierState {soldierCoord :: Coord,
                                  hasGrenade :: Bool}
                  deriving Show
                    
data Dir = S | D | U | L | R
         deriving Show

type Trans = (Int,Int)

trans :: Dir -> Trans
trans S = (0,0)
trans U = (0,-1)
trans D = (0,1)
trans L = (-1,0)
trans R = (1,0)

manhattan (a,b) (c,d) = abs (a-c) + abs (b-d)
               
(<+>) :: Coord -> Trans -> Coord
(a,b) <+> (c,d) = (a+c,b+d)

move :: Dir -> Coord -> Coord
move d c = c <+> trans d

type Name = String

data Grenade = Grenade {grenadeCoord :: Coord, countdown :: Int}
               deriving Show

data Command = Command {commandDirection :: Dir,
                        throwsGrenade :: Maybe Coord}
               deriving Show

type ProcessM = RWS Game [Grenade] ()

runProcessM :: Game -> ProcessM a -> (a,[Grenade])
runProcessM g x = evalRWS x g ()

validCoordinate :: Game -> Coord -> Bool
validCoordinate g (x,y) = x >= 0 && y >= 0 && x < w && y < h
  where (w,h) = size . board $ g

moveSoldier :: Command -> SoldierState -> ProcessM SoldierState
moveSoldier (Command d t) (SoldierState c g) = 
  do game <- ask
     let to = move d c
         ok = validCoordinate game to
     return $ SoldierState (if ok then to else c) g

throw :: Command -> SoldierState -> ProcessM SoldierState
throw c st = case throwsGrenade c
             of Nothing -> return st
                Just coord
                  | canThrow st coord -> tell [Grenade coord 2]
                                         >> return st {hasGrenade = False}
                  | otherwise -> return st

canThrow :: SoldierState -> Coord -> Bool
canThrow s c = hasGrenade s
               && manhattan (soldierCoord s) c <= 10
               && hasGrenade s

processSoldier :: Command -> SoldierState -> ProcessM SoldierState
processSoldier c = throw c >=> moveSoldier c

processSoldiers :: M.Map Name SoldierState -> M.Map Name Command -> ProcessM (M.Map Name SoldierState)
processSoldiers ss cs = foldM f ss $ M.assocs cs
  where f :: M.Map Name SoldierState -> (Name,Command) -> ProcessM (M.Map Name SoldierState)
        f ss (name,command) =
          case (M.lookup name ss)
          of Nothing -> return ss
             (Just s) -> M.insert name <$> processSoldier command s <*> pure s

processGrenades :: [Grenade] -> ([Grenade],[Explosion])
processGrenades gs = (remaining,explosions)
  where news = map (\g -> g {countdown = countdown g - 1}) gs
        (exploded,remaining) = partition ((==0).countdown) news
        explosions = map grenadeCoord exploded

data Team = Team {soldiers :: M.Map Name SoldierState}
            deriving Show

type Explosion = Coord

kills :: Explosion -> SoldierState -> Bool
kills (a,b) (SoldierState (c,d) _) = abs (a-c) <= 1 && abs (b-d) <= 1

updateTeam :: Team
              -> [Explosion] 
              -> M.Map Name Command
              -> ProcessM Team
updateTeam (Team solds) explosions commands =
  Team <$> processSoldiers surviving commands
  where surviving = M.filter (\s -> not $ any (flip kills s) explosions) solds

data Board = Board {size :: (Int,Int)}
             deriving Show

data Game = Game {board :: Board, ateam :: Team, bteam :: Team, grenades :: [Grenade]}
            deriving Show

updateGame :: Game -> M.Map Name Command -> M.Map Name Command -> Game
updateGame g@(Game b at bt gs) acommand bcommand = Game b at' bt' gs'
  where (gremaining,explosions) = processGrenades gs
        ((at',bt'),gs') = runProcessM g
                          $ tell gremaining >>
                            (,)
                            <$> updateTeam at explosions acommand
                            <*> updateTeam bt explosions bcommand

{-
-- WIP

applyExplosion :: Explosion -> Team -> Team
applyExplosion expl t = t { soldiers = surviving }
  where surviving = M.filter (\s -> not $ kills expl s) $ soldiers t
        
data Event = EExplode Explosion | EGrenade Grenade | ECommand Name Command

apply :: Game -> Event -> Game
apply (EExplode e) game = game {ateam = applyExplosion e $ ateam g,
                                bteam = applyExplosion e $ bteam g}
apply (EGrenade g) game = game {grenades = g:grenades}
apply (ECommand c) game = undefined
                                  
-}

runGame :: Game -> [(M.Map Name Command, M.Map Name Command)] -> [Game]
runGame = scanl upd 
  where upd g (ca,cb) = updateGame g ca cb
        
drawGame' :: Game -> M.Map Coord String
drawGame' g = M.unionsWith (\x y -> x ++ "," ++ y) $ map M.fromList [gs,ta,tb]
  where gs = map drawGrenade $ grenades g
        drawGrenade (Grenade c t) = (c,show t)
        ta = map drawSoldier . M.assocs . soldiers $ ateam g
        tb = map drawSoldier . M.assocs . soldiers $ bteam g
        drawSoldier (name,s) = (soldierCoord s,name)
        
drawGame :: Game -> String
drawGame g = intercalate "\n" $ map (intercalate " " . map d) coords
  where (w,h) = size . board $ g
        coords = map (\x -> map ((,)x) [0..h-1]) [0..w-1]
        drawn = drawGame' g
        d c = M.findWithDefault "." c drawn
        
