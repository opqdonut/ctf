module Input where

import Engine

import Data.Function
import Data.List
import Control.Applicative

readCmd :: String -> Command
readCmd s =
  let name:sdir:scoord = words s
      dir = read sdir
      coord = case scoord of [] -> Nothing
                             [s] -> Just (read s)
  in Command name dir coord
     
verifyCmds :: [Command] -> Bool
verifyCmds cs = cs == nubBy ((==) `on` commandName) cs

readCmds :: IO [Command]
readCmds = do l <- getLine
              case l of "" -> return []
                        s  -> (readCmd s:) <$> readCmds
