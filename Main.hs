module Main where

import Engine
import Input

import Control.Monad
import qualified Data.Map as M

-- -- -- -- -- -- -- -- -- -- --

sampleGame = Game (Board (11,11) respawn) (M.union ta tb) []
  where ta = M.fromList [("A", SoldierState "A" A (0,0) True),
                         ("B", SoldierState "B" A (0,1) True),
                         ("C", SoldierState "C" A (0,2) True)]
        tb = M.fromList [("D", SoldierState "D" B (10,0) True),
                         ("E", SoldierState "E" B (10,1) True),
                         ("F", SoldierState "F" B (10,2) True)]
        respawn A = (0,0)
        respawn B = (10,0)
             
sampleACommands g = [Command "A" R Nothing,
                     Command "B" R Nothing,
                     Command "C" D g]

sampleBCommands = [Command "D" S Nothing,
                   Command "E" L Nothing,
                   Command "F" L Nothing]
                                 
printGame x = putStrLn (drawGame x) >> putStrLn "--"
                  
step game = liftM (updateGame game) readCmds

run game = do game' <- step game
              printGame game'
              run game'

main = printGame sampleGame >> run sampleGame
