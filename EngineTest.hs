module Main where

import Engine
import Input

import Data.List
import Data.Maybe
import Control.Applicative
import Data.Function
import Test.QuickCheck
--import Control.Monad

type Check = Game -> Property

checkFlag :: Check
checkFlag g =
  let fs = fromFlags $ flags g
      fts = map flagTeam fs
      ok1 = length fts == 2 && elem A fts && elem B fts
      sfts = concatMap maybeToList . map holdsFlag . fromSoldiers $ soldiers g
      sftsA = filter (==A) sfts
      sftsB = filter (==B) sfts
      ok2 = length sftsA <= 1 && length sftsB <= 1
  in printTestCase "ok1" ok1 .&&. printTestCase "ok2" ok2
     
checkSoldiers :: Check
checkSoldiers g =
  let ss = fromSoldiers $ soldiers g
      check s = printTestCase ("checkSoldier:\n"++show s) $
                conjoin
                 [soldierCooldown s <= grenadeCooldown (rules g),
                  soldierCooldown s >= 0]
  in conjoin (map check ss)

checkGame g = conjoin . map ($g) $
              [printTestCase "checkFlag" . checkFlag,
               printTestCase "checkSoldiers" . checkSoldiers]

arbitraryCoord :: Game -> Gen Coord
arbitraryCoord g = do x <- elements [0..w-1]
                      y <- elements [0..h-1]
                      return (x,y)
  where (w,h) = boardSize . board $ g

arbitraryMove :: Game -> Gen Command
arbitraryMove g = do n <- elements (map soldierName ss)
                     d <- elements [U,L,D,R,S]
                     g <- frequency [(5,return Nothing),
                                     (1,Just <$> arbitraryCoord g)]
                     return $ Command n d g
  where ss = fromSoldiers $ soldiers g
        
arbitraryMoves :: Game -> Gen [Command]
arbitraryMoves g = do ms <- vectorOf 15 $ arbitraryMove g
                      return $ nubBy ((==)`on`commandName) ms
                      
testGame :: Int -> Game -> Property
testGame 0 g = checkGame g
testGame n g = do moves <- arbitraryMoves g
                  printTestCase ("testGame:\n"++show g) (checkGame g)
                    .&&. testGame (n-1) (updateGame g moves)
                           
main = do (rules,board) <- readConfFile "config"
          let g = mkGame board rules
          quickCheck $ testGame (nRounds rules) g