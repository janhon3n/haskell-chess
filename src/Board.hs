module Board
where

import Data.Array.IArray
import Data.Maybe
import Util

data Side = Black | White deriving (Eq, Show)
opposite Black = White
opposite White = Black

data PieceType = Soldier | Tower | Horse | Bishop | Queen | King deriving (Eq, Show)
data Piece = Piece {getSide :: Side, getPieceType :: PieceType} deriving (Eq, Show)

type Board = Array (Int, Int) (Maybe Piece)
type Place = (Int, Int)
type RelativePlace = (Int, Int)

instance (Integral a, Integral b) => Num (a,b) where
   (+) a b = (fst a + (fst b), snd a + (snd b))
   (*) a b = (fst a * (fst b), snd a * (snd b))

emptyBoard :: Board
emptyBoard = array ((1,1), (8,8)) $
      [((i,j), Nothing) | i <- [1..8], j <- [1..8]]

initialPlaces :: Side -> [(Place, Maybe Piece)]
initialPlaces side =
      map makePiece $
            [((i,j), Soldier) | i <- [2], j <- [1..8]]
            ++ [((1,1), Tower), ((1,8), Tower)]
            ++ [((1,2), Bishop), ((1,7), Bishop)]
            ++ [((1,3), Horse), ((1,6), Horse)]
            ++ [((1,4), Queen), ((1,5), King)]
            where makePiece ((i,j), piece) = case side of
                        Black -> ((i,j), Just (Piece side piece))
                        White -> ((9-i,j), Just (Piece side piece))

initialBoard :: Board
initialBoard = emptyBoard // initialPlaces Black // initialPlaces White

movePiece :: Board -> Place -> Place -> Board
movePiece board from to = board // [(to, board ! from), (from, Nothing)]

getPieces :: Board -> Side -> [(Place, Piece)]
getPieces board side = map (\p -> (p, fromJust (board ! p))) $ filter
         (\p -> case board ! p of
            Just (Piece side _) -> True
            _ -> False
         ) [ (i,j) | i <- [1..8], j <- [1..8]]

{- If given a list of relative places from the first quadrin (positive, positive)
returns a list with places from all quadrins.
Uses mirroing over the axes, so input must be symmetrical to x=y line -}
allRotations :: [RelativePlace] -> [RelativePlace]
allRotations places = removeDuplicates $ concat $ map
            (\(i,j) -> [(i,j), (i * (-1), j), (i * (-1), j* (-1)), (i, j * (-1))])
               $ filter (\(i,j) -> i >= 0 && j >= 0) places

