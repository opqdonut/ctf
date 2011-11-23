module Main where

import Engine
import Input

import System.IO

-- -- -- -- -- -- -- -- -- -- --

sampleBoard = readBoard
              ["A..............",
               "...............",
               ".........#.....",
               "...###...#.....",
               "...###...#.....",
               "...#.....#.....",
               ".a......###....",
               "...............",
               "..###..........",
               "....#........b.",
               "....#....##....",
               "....#..####....",
               ".......####....",
               ".......####....",
               "..............B"]

sampleGame = mkGame sampleBoard ["A","B","C"] ["D","E","F"]
                                 
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
