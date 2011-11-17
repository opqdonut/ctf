module Engine where

import qualified Data.Map as M
import Control.Monad
import Control.Applicative
import Data.List
import Data.Array
import Data.Maybe
import Control.Monad.Writer

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

moveSoldier :: Command -> SoldierState -> SoldierState
moveSoldier (Command d t) (SoldierState c g) = SoldierState (move d c) g

processSoldier :: SoldierState -> Command -> Writer [Grenade] SoldierState
processSoldier st c = do hasG <- throw st c
                         let moved = moveSoldier c st
                         return moved {hasGrenade = hasG}

throw :: SoldierState -> Command -> Writer [Grenade] Bool
throw st c = case throwsGrenade c
             of Nothing -> return (hasGrenade st)
                Just coord
                  | canThrow st coord -> tell [Grenade coord 2] >> return False
                  | otherwise -> return (hasGrenade st)

canThrow :: SoldierState -> Coord -> Bool
canThrow s c = hasGrenade s
               && manhattan (soldierCoord s) c <= 10
               && hasGrenade s

processSoldiers :: M.Map Name SoldierState -> M.Map Name Command -> Writer [Grenade] (M.Map Name SoldierState)
processSoldiers ss cs = foldM f ss $ M.assocs cs
  where f :: M.Map Name SoldierState -> (Name,Command) -> Writer [Grenade] (M.Map Name SoldierState)
        f ss (name,command) = case (M.lookup name ss)
                              of Nothing -> return ss
                                 (Just s) -> do s' <- processSoldier s command
                                                return $ M.insert name s' ss
                             
processCommands :: M.Map Name SoldierState -> M.Map Name Command -> (M.Map Name SoldierState, [Grenade])
processCommands soldiers commands = runWriter $ processSoldiers soldiers commands

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
              -> M.Map Name Command -- ^ commands
              -> (Team, [Grenade])
updateTeam (Team solds) explosions commands = (Team new, gs)
  where surviving = M.filter (\s -> not $ any (flip kills s) explosions) solds
        (new,gs) = processCommands surviving commands

data Board = Board {size :: (Int,Int)}
             deriving Show

data Game = Game {board :: Board, ateam :: Team, bteam :: Team, grenades :: [Grenade]}
            deriving Show

updateGame :: Game -> M.Map Name Command -> M.Map Name Command -> Game
updateGame (Game b at bt gs) acommand bcommand = Game b at' bt' gs'
  where (gremaining,explosions) = processGrenades gs
        (at',ga) = updateTeam at explosions acommand
        (bt',gb) = updateTeam bt explosions bcommand
        gs' = gremaining ++ ga ++ gb
        
drawGame' :: Game -> M.Map Coord String
drawGame' g = M.fromList $ bg ++ gs ++ ta ++ tb
  where bg = []
        gs = map drawGrenade $ grenades g
        drawGrenade (Grenade c t) = (c,show t)
        ta = map drawSoldier . M.assocs . soldiers $ ateam g
        tb = map drawSoldier . M.assocs . soldiers $ bteam g
        drawSoldier (name,s) = (soldierCoord s,name)
        
drawGame :: Game -> String
drawGame g = intercalate "\n" $ map (concatMap d) coords
  where (w,h) = size . board $ g
        coords = map (\x -> map ((,)x) [0..h-1]) [0..w-1]
        drawn = drawGame' g
        d c = M.findWithDefault "." c drawn
        
