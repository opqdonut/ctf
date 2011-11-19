module Engine where

import qualified Data.Map as M
import Control.Monad
import Control.Applicative
import Data.List
import Data.Array
import Data.Maybe
import Control.Monad.RWS
import Control.Monad.Writer

type Coord = (Int,Int)

data SoldierState = SoldierState {soldierName :: Name,
                                  soldierCoord :: Coord,
                                  hasGrenade :: Bool}
                  deriving Show

type Soldiers = M.Map Name SoldierState
                           
data Dir = S | D | U | L | R
         deriving (Show,Read,Eq)

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

data Command = Command {commandName :: Name,
                        commandDirection :: Dir,
                        throwsGrenade :: Maybe Coord}
               deriving (Show,Eq)

type ProcessM = RWS Game [Grenade] ()

runProcessM :: Game -> ProcessM a -> (a,[Grenade])
runProcessM g x = evalRWS x g ()

validCoordinate :: Coord -> Game -> Bool
validCoordinate (x,y) g = x >= 0 && y >= 0 && x < w && y < h
  where (w,h) = size . board $ g

moveSoldier :: Command -> SoldierState -> ProcessM SoldierState
moveSoldier (Command _ d t) ss = 
  do let to = move d (soldierCoord ss)
     ok <- asks $ validCoordinate to
     return $ if ok
              then ss {soldierCoord = to}
              else ss

throw :: Command -> SoldierState -> ProcessM SoldierState
throw (Command _ _ Nothing) st = return st
throw (Command _ _ (Just coord)) st =
  do cOk <- asks $ validCoordinate coord
     let tOk = canThrow st coord
     if (cOk && tOk)
       then tell [Grenade coord 2] >> return st {hasGrenade = False}
       else return st

canThrow :: SoldierState -> Coord -> Bool
canThrow s c = hasGrenade s
               && manhattan (soldierCoord s) c <= 10
               && hasGrenade s

processSoldier :: Command -> SoldierState -> ProcessM SoldierState
processSoldier c = throw c >=> moveSoldier c

processCommands :: [Command]
                   -> Soldiers
                   -> ProcessM Soldiers
processCommands cs ss = foldM f ss $ cs
  where f :: Soldiers -> Command -> ProcessM Soldiers
        f ss command =
          let name = commandName command in
          case M.lookup name ss
          of Nothing  -> return ss
             (Just s) -> do s' <- processSoldier command s
                            return $ M.insert name s' ss

processGrenades :: [Grenade] -> ([Grenade],[Explosion])
processGrenades gs = (remaining,explosions)
  where news = map (\g -> g {countdown = countdown g - 1}) gs
        (exploded,remaining) = partition ((==0).countdown) news
        explosions = map grenadeCoord exploded

type Explosion = Coord

kills :: SoldierState -> Explosion -> Bool
kills s (a,b) = abs (a-c) <= 1 && abs (b-d) <= 1
  where (c,d) = soldierCoord s

processExplosions :: [Explosion]
                     -> Soldiers -> Soldiers
processExplosions explosions solds =
  M.filter (\s -> not $ any (kills s) explosions) solds

updateSoldiers :: [Explosion] 
                  -> [Command]
                  -> Soldiers
                  -> ProcessM Soldiers
updateSoldiers explosions commands =
  processCommands commands
  . processExplosions explosions

data Board = Board {size :: (Int,Int)}
             deriving Show

data Game = Game {board :: Board,
                  soldiers :: Soldiers,
                  grenades :: [Grenade]}
            deriving Show

updateGame :: Game -> [Command] -> Game
updateGame g@(Game b ss gs) commands = Game b ss' gs'
  where (gremaining,explosions) = processGrenades gs
        (ss',gs') = runProcessM g
                          $ tell gremaining >>
                            updateSoldiers explosions commands ss

{-
runGame :: Game -> [([Command], [Command])] -> [Game]
runGame = scanl upd 
  where upd g (ca,cb) = updateGame g ca cb
-}
        
drawGame' :: Game -> M.Map Coord String
drawGame' g = M.fromListWith comb (gs++tss)
  where comb x y = x ++ "," ++ y
        gs = map drawGrenade $ grenades g
        drawGrenade (Grenade c t) = (c,show t)
        tss = map drawSoldier . M.assocs . soldiers $ g
        drawSoldier (name,s) = (soldierCoord s,name)
        
drawGame :: Game -> String
drawGame g = intercalate "\n" $ map (intercalate " " . map d) coords
  where (w,h) = size . board $ g
        coords = map (\x -> map ((,)x) [0..h-1]) [0..w-1]
        drawn = drawGame' g
        d c = M.findWithDefault "." c drawn

