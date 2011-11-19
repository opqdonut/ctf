module Main where

import Engine
import Input

import Control.Monad
import qualified Data.Map as M

-- -- -- -- -- -- -- -- -- -- --

sampleGame = Game (Board (11,11)) (M.union ta tb) []
  where ta = M.fromList [("A", SoldierState "A" (0,0) True),
                         ("B", SoldierState "B" (0,1) True),
                         ("C", SoldierState "C" (0,2) True)]
        tb = M.fromList [("D", SoldierState "D" (10,0) True),
                         ("E", SoldierState "E" (10,1) True),
                         ("F", SoldierState "F" (10,2) True)]
             
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
