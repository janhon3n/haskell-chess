module Player
where
import Board

data PlayerType = EvaluatingAI | TreeAI deriving (Eq, Show)

evaluateBoard :: Board -> Double
evaluateBoard board = 0