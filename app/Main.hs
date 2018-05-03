module Main where

import Chess
import Board
import Move

main :: IO ()
main = do
   print $ getAllPossibleMoves initialBoard Black
   return ()
