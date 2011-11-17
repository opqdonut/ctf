module Engine where

import qualified Data.Map as M
import Control.Monad
import Control.Applicative
import Data.List
import Data.Array
import Data.Maybe
--import Control.Monad.Writer

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

moveSoldiers :: M.Map Name SoldierState -> M.Map Name Command -> M.Map Name SoldierState
moveSoldiers ss cs = foldl' f ss $ M.assocs cs
  where f ss (name,command) = M.update (Just . moveSoldier command) name ss

canThrow :: SoldierState -> Coord -> Bool
canThrow s c = manhattan (soldierCoord s) c <= 10
               && hasGrenade s

getGrenades :: M.Map Name SoldierState -> M.Map Name Command -> [Grenade]
getGrenades ss m = do (name,c) <- M.assocs m
                      case throwsGrenade c
                        of (Just coord)
                             | canThrow (fromJust $ M.lookup name ss) coord -> [Grenade coord 2]
                             | otherwise -> []
                           Nothing -> []
                             
processCommands :: M.Map Name SoldierState -> M.Map Name Command -> (M.Map Name SoldierState, [Grenade])
processCommands soldiers commands = (new, getGrenades new commands)
  where new = moveSoldiers soldiers commands

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
        
-- -- -- -- -- -- -- -- -- -- --

sampleGame = Game (Board (11,11)) ta tb []
  where ta = Team $ M.fromList [("A", SoldierState (0,0) True),
                                ("B", SoldierState (0,1) True),
                                ("C", SoldierState (0,2) True)]
        tb = Team $ M.fromList [("D", SoldierState (10,0) True),
                                ("E", SoldierState (10,1) True),
                                ("F", SoldierState (10,2) True)]
             
sampleACommands g = M.fromList [("A", Command R Nothing),
                                ("B", Command R Nothing),
                                ("C", Command D g)]

sampleBCommands = M.fromList [--("D", Command L Nothing),                  
                              ("E", Command L Nothing),
                              ("F", Command L Nothing)]
                                 
                  
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
        

main = let f x = putStrLn (drawGame x) >> putStrLn "--"
           g = sampleGame
           g' = updateGame g (sampleACommands (Just (8,3))) sampleBCommands
           g'' = updateGame g' (sampleACommands Nothing) sampleBCommands
           g''' = updateGame g'' (sampleACommands (Just (0,0))) sampleBCommands
           g'''' = updateGame g''' (sampleACommands Nothing) sampleBCommands
       in mapM_ f [g,g',g'',g''',g'''']
              