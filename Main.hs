module Main where

import Engine
import Input

import Data.Array
import System.IO

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
              putStrLn "pending:" >>
              putStrLn (gamePending x) >>
              putStrLn "--" 
                  
step game = do ca <- queryCmds stdout stdin game A
               cb <- queryCmds stdout stdin game B
               return . updateGame game $ ca ++ cb

run game = do game' <- step game
              printGame game'
              run game'

main = printGame sampleGame >> run sampleGame
