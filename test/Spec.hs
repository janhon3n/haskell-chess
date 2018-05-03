import Test.HUnit
import Data.List
import Board
import Move

getLinePlacesTest = TestCase (assertEqual "Line segment found correctly"
   (sort [(3,2), (4,2), (5,2), (6,2), (7,2)])
   (sort $ getLinePlaces initialBoard (2,2) (1,0) Black)
   )


tests = TestList [
            TestLabel "getLinePlacesTest" getLinePlacesTest
         ]

main :: IO ()
main = do
   runTestTT tests
   return ()