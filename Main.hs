module Main where

import Engine
import Input

import Control.Monad
import Data.Array

-- -- -- -- -- -- -- -- -- -- --

sampleBoard = Board (listArray ((0,0),(14,14)) l) respawnf
  where respawnf A = (0,0)
        respawnf B = (14,14)
        f x = case x of '#' -> Obstacle
                        '.' -> Empty
                        _ -> error "evo"
        l = map f
            "...............\
            \...............\
            \.........#.....\
            \...###...#.....\
            \...###...#.....\
            \...#.....#.....\
            \........###....\
            \...............\
            \..###..........\
            \....#..........\
            \....#....##....\
            \....#..####....\
            \.......####....\
            \.......####....\
            \..............."

sampleGame = mkGame sampleBoard ["A","B","C"] ["D","E","F"]
             
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
