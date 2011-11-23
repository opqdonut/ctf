module Input where

import Engine

import Data.List
import Control.Applicative
import System.IO
import qualified Data.Map as M

readCmd :: String -> Command
readCmd str =
  let name:sdir:scoord = words str
      dir = read sdir
      coord = case scoord of [] -> Nothing
                             [s] -> Just (read s)
                             _ -> error "malformed command!"
  in Command name dir coord
     
verifyCmds :: Game -> Team -> [Command] -> Bool
verifyCmds game t cs = distinct && own
  where names = map commandName cs
        distinct = names == nub names
        own = all (\n -> Just t == fmap soldierTeam (M.lookup n (soldiers game))) names

getCmds :: Handle -> IO [Command]
getCmds h = do l <- hGetLine h
               case l of "" -> return []
                         s  -> (readCmd s:) <$> getCmds h

queryCmds :: Handle -> Handle -> Game -> Team -> IO [Command]
queryCmds hout hin g t = do hPutStrLn hout $ gameInfo g t
                            cs <- getCmds hin
                            if verifyCmds g t cs
                              then return cs
                              else error "commands not valid!"
