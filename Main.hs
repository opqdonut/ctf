module Main where

import Engine
import Input

import System.IO
import System.Environment
import System.Process
import Data.Array

printGame x = putStrLn (drawGame x DrawAll) >>
              putStrLn "objects:" >>
              putStrLn (gameObjects x) >>
              putStrLn "pending:" >>
              putStrLn (gamePending x) >>
              putStrLn "--" 
                  
printStats x = do putStr "Final score: "
                  putStrLn $ "A " ++ show (points x ! A) ++ " - B " ++ show (points x ! B)
              
step game = do ca <- queryCmds stdout stdin game A
               cb <- queryCmds stdout stdin game B
               return . updateGame game $ ca ++ cb

run game = do game' <- step game
              printGame game'
              run game'

readConfFile :: String -> IO (Rules,Board)
readConfFile confFile = do (srules:smap) <- fmap lines $ readFile confFile
                           let rules = read srules
                               board = readBoard smap
                           return (rules, board)

main = do
  (confFile:cmd1:cmd2:_) <- getArgs
  (rules,board) <- readConfFile confFile
  (out1,in1,err1,_ph1) <- runInteractiveCommand cmd1
  (out2,in2,err2,_ph2) <- runInteractiveCommand cmd2

  mapM_ (\h -> hSetBuffering h LineBuffering) [out1,in1,out2,in2,err1,err2]

  let step g = do printGame g
                  c1 <- queryCmds out1 in1 g A
                  c2 <- queryCmds out2 in2 g B
                  return . updateGame g $ c1++c2
      run 0 g = printStats g
      run n g = step g >>= run (n-1)
      game = mkGame board rules ["X","Y","Z"] ["L","M","N"]

  hPutStrLn out1 (drawBoard board)
  hPutStrLn out2 (drawBoard board)

  run (nRounds rules) game

main2 = do
  confFile:_ <- getArgs
  
  (rules,board) <- readConfFile confFile
  
  let game = mkGame board rules ["A","B","C"] ["D","E","F"]
  
  printGame game >> run game
