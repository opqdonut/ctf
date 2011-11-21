module Main where

import Engine
import Input

import Control.Monad
import qualified Data.Map as M

-- -- -- -- -- -- -- -- -- -- --

sampleGame = Game (Board (11,11) respawnf) (M.union ta tb) []
  where ta = M.fromList [("A", mkSoldier "A" A (0,0)),
                         ("B", mkSoldier "B" A (0,1)),
                         ("C", mkSoldier "C" A (0,2))]
        tb = M.fromList [("D", mkSoldier "D" B (10,0)),
                         ("E", mkSoldier "E" B (10,1)),
                         ("F", mkSoldier "F" B (10,2))]
        respawnf A = (0,0)
        respawnf B = (10,0)
             
sampleACommands g = [Command "A" R Nothing,
                     Command "B" R Nothing,
                     Command "C" D g]

sampleBCommands = [Command "D" S Nothing,
                   Command "E" L Nothing,
                   Command "F" L Nothing]
                                 
printGame x = putStrLn (drawGame x) >>
              putStrLn "A:" >>
              putStrLn (gameInfo x A) >>
              putStrLn "B:" >>
              putStrLn (gameInfo x B) >>
              putStrLn "pending:" >>
              putStrLn (gamePending x) >>
              putStrLn "--" 
                  
step game = liftM (updateGame game) readCmds

run game = do game' <- step game
              printGame game'
              run game'

main = printGame sampleGame >> run sampleGame
