module Input where

import Engine

import Data.Function
import Data.List
import Control.Applicative

readCmd :: String -> Command
readCmd str =
  let name:sdir:scoord = words str
      dir = read sdir
      coord = case scoord of [] -> Nothing
                             [s] -> Just (read s)
                             _ -> error "malformed command!"
  in Command name dir coord
     
verifyCmds :: [Command] -> Bool
verifyCmds cs = cs == nubBy ((==) `on` commandName) cs

readCmds :: IO [Command]
readCmds = do l <- getLine
              case l of "" -> return []
                        s  -> (readCmd s:) <$> readCmds
