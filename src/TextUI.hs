module TextUI
where
import Board
import Data.Array.IArray

renderPiece :: Piece -> String
renderPiece (Piece side piecetype) =
   take 1 (show side) ++ (take 1 (show piecetype))

renderBoard :: Board -> String
renderBoard board =
   foldl (\s ind -> do
      let endstr = case ind of
            (i,8) -> "\n"
            i -> "|"
      case board ! ind of
         Just piece -> s ++ renderPiece piece ++ endstr
         Nothing -> s ++ "  " ++ endstr
      )
      ""
      [(i,j) | i <- [1..8], j <- [1..8]]