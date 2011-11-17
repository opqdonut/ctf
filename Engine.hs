module Engine where

import qualified Data.Map as M
import Control.Monad
import Control.Applicative
import Data.List
--import Control.Monad.Writer

type Coord = (Int,Int)

data SoldierState = SoldierState {soldierCoord :: Coord
                                  {-hasGrenade :: Bool-} }
                  deriving Show
                    
data Dir = D | U | L | R
         deriving Show

type Trans = (Int,Int)

trans :: Dir -> Trans
trans U = (0,-1)
trans D = (0,1)
trans L = (-1,0)
trans R = (1,0)
               
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

moveSoldier :: SoldierState -> Command -> SoldierState
moveSoldier (SoldierState c) (Command d _) = SoldierState $ move d c

updateSoldiers :: M.Map Name SoldierState -> M.Map Name Command -> M.Map Name SoldierState
updateSoldiers = M.intersectionWith moveSoldier

getGrenades :: M.Map Name Command -> [Grenade]
getGrenades m = do c <- M.elems m
                   case throwsGrenade c of (Just coord) -> [Grenade coord 2]
                                           Nothing -> []
                             
processCommands :: M.Map Name SoldierState -> M.Map Name Command -> (M.Map Name SoldierState, [Grenade])
processCommands soldiers commands = (updateSoldiers soldiers commands, getGrenades commands)

processGrenades :: [Grenade] -> ([Grenade],[Explosion])
processGrenades gs = (remaining,explosions)
  where news = map (\g -> g {countdown = countdown g - 1}) gs
        (exploded,remaining) = partition ((==0).countdown) news
        explosions = map grenadeCoord exploded

data Team = Team {soldiers :: M.Map Name SoldierState}
            deriving Show

type Explosion = Coord

kills :: Explosion -> SoldierState -> Bool
kills (a,b) (SoldierState (c,d)) = abs (a-c) <= 1 && abs (b-d) <= 1

updateTeam :: Team
              -> [Explosion] 
              -> M.Map Name Command -- ^ commands
              -> (Team, [Grenade])
updateTeam (Team solds) explosions commands = (Team new, gs)
  where surviving = M.filter (\s -> not $ any (flip kills s) explosions) solds
        (new,gs) = processCommands surviving commands

data Game = Game {ateam :: Team, bteam :: Team, grenades :: [Grenade]}
            deriving Show

updateGame :: Game -> M.Map Name Command -> M.Map Name Command -> Game
updateGame (Game at bt gs) acommand bcommand = Game at' bt' gs'
  where (gremaining,explosions) = processGrenades gs
        (at',ga) = updateTeam at explosions acommand
        (bt',gb) = updateTeam bt explosions bcommand
        gs' = gremaining ++ ga ++ gb
        
        

sampleGame = Game ta tb []
  where ta = Team $ M.fromList [("A", SoldierState (0,0)),
                                ("B", SoldierState (0,1)),
                                ("C", SoldierState (0,2))]
        tb = Team $ M.fromList [("D", SoldierState (10,0)),
                                ("E", SoldierState (10,1)),
                                ("F", SoldierState (10,2))]
             
sampleACommands g = M.fromList [("A", Command R Nothing),
                                ("B", Command R Nothing),
                                ("C", Command D g)]

sampleBCommands = M.fromList [("D", Command L Nothing),                  
                              ("E", Command L Nothing),
                              ("F", Command L Nothing)]
                                 