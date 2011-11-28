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

main = do
  (confFile:cmd1:cmd2:_) <- getArgs
  (rules,board) <- readConfFile confFile
  (out1,in1,err1,ph1) <- runInteractiveCommand cmd1
  (out2,in2,err2,ph2) <- runInteractiveCommand cmd2

  mapM_ (\h -> hSetBuffering h LineBuffering) [out1,in1,out2,in2,err1,err2]

  hPutStrLn out1 (show A)
  hPutStrLn out1 (drawBoard board)
  hPutStrLn out2 (show B)
  hPutStrLn out2 (drawBoard board)

  let step g = do printGame g
                  c1 <- queryCmds out1 in1 g A
                  c2 <- queryCmds out2 in2 g B
                  return . updateGame g $ c1++c2
      run 0 g = printStats g
      run n g = step g >>= run (n-1)
      game = mkGame board rules

  run (nRounds rules) game

  mapM_ hClose [out1,in1,out2,in2,err1,err2]
  mapM_ terminateProcess [ph1,ph2]
