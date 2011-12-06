module Input where

import Engine

import Data.List
import Data.Array
import Control.Applicative
import Control.Monad
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
                            putStrLn $ show t++": "++show cs
                            if verifyCmds g t cs
                              then return cs
                              else error "commands not valid!"

readBoard :: [String] -> Board
readBoard strs = if ok
                 then Board $ listArray ((0,0),(w-1,h-1)) tiles
                 else error "line lengths in board don't match"
  where h = length strs
        w = length (head strs)
        ok = all (==w) $ map length strs
        tiles = map f (concat $ transpose strs)
        f x = case x of '#' -> Obstacle
                        '.' -> Empty
                        'A' -> Spawn A
                        'B' -> Spawn B
                        'a' -> Base A
                        'b' -> Base B
                        _ -> error $ "invalid tile: " ++ show x
  
readConfFile :: String -> IO (Rules,Board)
readConfFile confFile = do (srules:smap) <- fmap lines $ readFile confFile
                           let rules = read srules
                               board = readBoard smap
                           
                           when (nSoldiers rules > maxSoldiers) $ do
                             fail "nSoldiers is too big!"
                             
                           return (rules, board)
