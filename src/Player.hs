module Player
where

data PlayerType = EvaluatingAI | TreeAI deriving (Eq, Show)

chooseMove ::  Board -> PlayerType -> Move
chooseMove board EvaluatingAI =

evaluateBoard :: Board -> Double
evaluateBoard board = 0