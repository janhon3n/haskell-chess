module Chess
where

import Board

data GameState = GameState Board Side

initialState :: GameState
initialState = GameState initialBoard White

playTurn :: GameState -> Maybe GameState
playTurn (GameState board sideInTurn) = return $ GameState board sideInTurn

