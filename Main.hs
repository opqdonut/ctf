module Main where

import Engine
import Input

import Control.Monad
import Control.Concurrent
import System.IO
import System.IO.Error
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

spawn cmd = createProcess (shell cmd) {std_in = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = CreatePipe}

pipePrefixed :: String -> Handle -> Handle -> IO ()
pipePrefixed str hin hout = forever
                            (do s <- hGetLine hin
                                hPutStrLn hout (str++s))
                            `catch`
                            (\e -> if isEOFError e
                                   then hPutStrLn hout (str++"<<EOF>>")
                                   else hPutStrLn hout "something else!" >> ioError e)

main = do
  (confFile:cmd1:cmd2:_) <- getArgs
  (rules,board) <- readConfFile confFile
  (Just out1,Just in1,Just err1,ph1) <- spawn cmd1
  (Just out2,Just in2,Just err2,ph2) <- spawn cmd2

  mapM_ (\h -> hSetBuffering h LineBuffering) [out1,in1,out2,in2]

  forkIO $ pipePrefixed "A: " err1 stderr
  forkIO $ pipePrefixed "B: " err2 stderr

  hPutStrLn out1 (show A)
  hPutStrLn out1 (drawBoard board)
  hPutStrLn out2 (show B)
  hPutStrLn out2 (drawBoard board)

  let step g = do printGame g
                  c1 <- queryCmds out1 in1 g A
                     `catch`
                     (\e -> putStrLn "A failed" >> ioError e)
                  c2 <- queryCmds out2 in2 g B
                    `catch`
                     (\e -> putStrLn "B failed" >> ioError e)
                  return . updateGame g $ c1++c2
      run 0 g = printStats g
      run n g = step g >>= run (n-1)
      game = mkGame board rules

  run (nRounds rules) game

  mapM_ hClose [out1,in1,out2,in2]
  mapM_ terminateProcess [ph1,ph2]
