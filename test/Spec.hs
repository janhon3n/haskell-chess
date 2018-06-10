import Test.HUnit
import Data.List
import Board
import Move

getLinePlacesTest = TestCase (
   assertEqual "Line segment found correctly"
   (sort [(3,2), (4,2), (5,2), (6,2), (7,2)])
   (sort $ getLinePlaces initialBoard (2,2) (1,0) Black)
   )

allRotationsTest = TestCase (
   assertEqual "All rotations for list of places returned correctly"
   (sort [(0,1), (1,0), (2,2), (-1,0), (0,-1), (-2,2), (2,-2), (-2,-2)])
   (sort $ allRotations [(0,1), (1,0), (2,2)])
   )

tests = TestList [
            TestLabel "getLinePlacesTest" getLinePlacesTest,
            TestLabel "allRotationsTest" allRotationsTest
         ]

main :: IO ()
main = do
   runTestTT tests
   return ()