module Main where

import Engine
import qualified Data.Map as M

-- -- -- -- -- -- -- -- -- -- --

sampleGame = Game (Board (11,11)) ta tb []
  where ta = Team $ M.fromList [("A", SoldierState "A" (0,0) True),
                                ("B", SoldierState "B" (0,1) True),
                                ("C", SoldierState "C" (0,2) True)]
        tb = Team $ M.fromList [("D", SoldierState "D" (10,0) True),
                                ("E", SoldierState "E" (10,1) True),
                                ("F", SoldierState "F" (10,2) True)]
             
sampleACommands g = [Command "A" R Nothing,
                     Command "B" R Nothing,
                     Command "C" D g]

sampleBCommands = [Command "D" S Nothing,
                   Command "E" L Nothing,
                   Command "F" L Nothing]
                                 
main = let f x = putStrLn (drawGame x) >> putStrLn "--"
           comms = [(sampleACommands (Just (8,3)), sampleBCommands),
                    (sampleACommands Nothing, sampleBCommands),
                    (sampleACommands (Just (8,3)), sampleBCommands),
                    (sampleACommands Nothing, sampleBCommands),
                    (sampleACommands Nothing, sampleBCommands),
                    (sampleACommands Nothing, sampleBCommands),
                    (sampleACommands Nothing, sampleBCommands),
                    (sampleACommands Nothing, sampleBCommands),
                    (sampleACommands Nothing, sampleBCommands),
                    (sampleACommands Nothing, sampleBCommands),
                    (sampleACommands Nothing, sampleBCommands)
                    ]
       in mapM_ f $ runGame sampleGame comms
