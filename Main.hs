module Main where

import Engine
import qualified Data.Map as M

-- -- -- -- -- -- -- -- -- -- --

sampleGame = Game (Board (11,11)) ta tb []
  where ta = Team $ M.fromList [("A", SoldierState (0,0) True),
                                ("B", SoldierState (0,1) True),
                                ("C", SoldierState (0,2) True)]
        tb = Team $ M.fromList [("D", SoldierState (10,0) True),
                                ("E", SoldierState (10,1) True),
                                ("F", SoldierState (10,2) True)]
             
sampleACommands g = M.fromList [("A", Command R Nothing),
                                ("B", Command R Nothing),
                                ("C", Command D g)]

sampleBCommands = M.fromList [--("D", Command L Nothing),                  
                              ("E", Command L Nothing),
                              ("F", Command L Nothing)]
                                 
main = let f x = putStrLn (drawGame x) >> putStrLn "--"
           comms = [(sampleACommands (Just (8,3)), sampleBCommands),
                    (sampleACommands Nothing, sampleBCommands),
                    (sampleACommands (Just (8,3)), sampleBCommands),
                    (sampleACommands Nothing, sampleBCommands),
                    (sampleACommands Nothing, sampleBCommands),
                    (sampleACommands Nothing, sampleBCommands)]
       in mapM_ f $ runGame sampleGame comms
