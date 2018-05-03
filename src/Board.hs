module Board
where

import Data.Array.IArray
import Data.Maybe

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


initialBoard :: Board
initialBoard = array ((1,1), (8,8)) $
      [((i,j), Nothing) | i <- [3..6], j <- [1..8]]
      ++ [((i,j), Just (Piece Black Soldier)) | i <- [2], j <- [1..8]]
      ++ [((i,j), Just (Piece White Soldier)) | i <- [7], j <- [1..8]]
      ++ [((1,1), Just (Piece Black Tower)), ((1,2), Just (Piece Black Horse)), ((1,3), Just (Piece Black Bishop)), ((1,4), Just (Piece Black Queen))]
      ++ [((1,8), Just (Piece Black Tower)), ((1,7), Just (Piece Black Horse)), ((1,6), Just (Piece Black Bishop)), ((1,5), Just (Piece Black King))]
      ++ [((8,1), Just (Piece White Tower)), ((8,2), Just (Piece White Horse)), ((8,3), Just (Piece White Bishop)), ((8,4), Just (Piece White Queen))]
      ++ [((8,8), Just (Piece White Tower)), ((8,7), Just (Piece White Horse)), ((8,6), Just (Piece White Bishop)), ((8,5), Just (Piece White King))]

movePiece :: Board -> Place -> Place -> Board
movePiece board from to = board // [(to, board ! from), (from, Nothing)]

getPieces :: Board -> Side -> [(Place, Piece)]
getPieces board side = map (\p -> (p, fromJust (board ! p))) $ filter
         (\p -> case board ! p of
            Just (Piece side _) -> True
            _ -> False
         ) [ (i,j) | i <- [1..8], j <- [1..8]]

{- IF given a list of relative places from the first quadrin (positive, positive)
returns a (length * 4) list with places from all quadrins -}
allRotations :: [RelativePlace] -> [RelativePlace]
allRotations places = map head . group . sort $ concat $ map
            (\(i,j) -> [(i,j), (i * (-1), j), (i * (-1), j* (-1)), (i, j * (-1))])
               places
