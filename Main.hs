module Main where

import Engine
import Input

import System.IO
import System.Environment

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

main = do
  confFile:_ <- getArgs
  (srules:smap) <- fmap lines $ readFile confFile
  
  let rules = read srules
      board = readBoard smap
      game = mkGame board rules ["A","B","C"] ["D","E","F"]
  
  printGame game >> run game
