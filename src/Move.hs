module Move
where

import Data.Array.IArray
import Data.List
import Board

type Move = (Place, Place)

getAllPossibleMoves :: Board -> Side -> [Move]
getAllPossibleMoves board side = foldl
         (\list (place, piece) -> union list (moves board piece place))
            [] $ getPieces board side


(*-) :: Place -> [Place] -> [Place]
place *- places = map (+ place) places

moves :: Board -> Piece -> Place -> [Move]
moves board piece place = map ((,) place) $ possiblePlaces board piece place

possiblePlaces :: Board -> Piece -> Place -> [Place]
possiblePlaces board piece place = case piece of
   Piece s Soldier -> getSoldierPlaces board place s

   Piece side King -> filter
            (\p -> case board ! p of
               Just (Piece s _) | s == side -> False
               _ -> True
            )
            $ filter (inRange (bounds board)) $ allRotations [(1,0), (1,1), (0,1)]
   
   Piece side Horse -> filter 
            (\p -> case board ! p of
               Just (Piece s _) | s == side -> False
               _ -> True
            )
            $ filter (inRange (bounds board)) $ allRotations [(1,2), (2,1)]

   Piece side Tower -> getLinePlacesFromDirections board place
                           (allRotations [(1,0), (0,1)]) side

   Piece side Bishop -> getLinePlacesFromDirections board place
                           (allRotations [(1,1)]) side

   Piece side Queen -> getLinePlacesFromDirections board place
                           (allRotations [(1,0), (1,1), (0,1)]) side


type Direction = (Int, Int)

getSoldierPlaces :: Board -> Place -> Side -> [Place]
getSoldierPlaces board place side = 
   (case (board ! (place + front)) of
      Nothing -> case place of
         (2,_) -> place *- [front, front * (2,0)]
         _ -> [place + front]
      _ -> []
   ) ++
   (
      case (place + (front + (0,1))) of
         (i,j) | not (inRange (bounds board) (i,j)) -> []
         p -> case board ! p of
            Just (Piece s _) | s == opposite side -> [place + (front + (0,1))] 
            _ -> []
   ) ++ 
   (
      case (place + (front + (0,-1))) of
         (i,j) | not (inRange (bounds board) (i,j)) -> []
         p -> case board ! p of
            Just (Piece s _) | s == opposite side -> [place + (front + (0,-1))] 
            _ -> []
   ) where
         front = case side of
               Black -> (1,0)
               White -> (-1,0)

getLinePlacesFromDirections :: Board -> Place -> [Direction] -> Side -> [Place]
getLinePlacesFromDirections board place directions side = foldl
                     (\xs dir -> union xs (getLinePlaces board place dir side))
                           [] directions

getLinePlaces :: Board -> Place -> Direction -> Side -> [Place]
getLinePlaces board place direction side = getLinePlaces' board place direction side []

getLinePlaces' :: Board -> Place -> Direction -> Side -> [Place] -> [Place]
getLinePlaces' board place direction side foundPlaces =
   case inRange (bounds board) (place+direction) of
      False -> foundPlaces
      True -> case board ! (place + direction) of
            Nothing -> getLinePlaces' board (place + direction) direction side $ (place + direction) : foundPlaces
            Just (Piece s _) | s == side -> foundPlaces
            Just (Piece s _) | s == opposite side -> (place + direction) : foundPlaces


moveIsValid :: Board -> Move -> Bool
moveIsValid board move = True