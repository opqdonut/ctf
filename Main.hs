module Main where

import Engine
import Input

import Control.Monad
import Data.Array

-- -- -- -- -- -- -- -- -- -- --

sampleBoard = Board (listArray ((0,0),(14,14)) l)
  where f x = case x of '#' -> Obstacle
                        '.' -> Empty
                        'A' -> Spawn A
                        'B' -> Spawn B
                        'a' -> Base A
                        'b' -> Base B
                        _ -> error "evo"
        l = map f
            "A..............\
            \...............\
            \.........#.....\
            \...###...#.....\
            \...###...#.....\
            \...#.....#.....\
            \.a......###....\
            \...............\
            \..###..........\
            \....#........b.\
            \....#....##....\
            \....#..####....\
            \.......####....\
            \.......####....\
            \..............B"

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
