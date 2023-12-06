module Main (main) where

import MyLib
import ReadInput

{-
import Test.Tasty
import Test.Tasty.HUnit
import Data.List (isInfixOf)

faultyTest :: TestTree
faultyTest = testCase "Faulty Test" $ do
  let result = someFunc
  assertEqual "Expected failure" "1337" result

correctTest :: TestTree
correctTest = testCase "Correct Test" $ do
  let result = someFunc
  assertEqual "Expected success" "someFunc" result

tests :: TestTree
tests = testGroup "MyLib Tests" [faultyTest, correctTest]

-}


main :: IO ()
main = do
  games <- readFileAsLines  "./input/2023/day2.txt"
  let parsedGames = map parseGame games
      playedGames = map tail parsedGames
      gamesIds = getIndexes parsedGames
      availableCounts = ["12 red","13 green","14 blue"]

      checkedGames = map (map (compareCounts availableCounts)) playedGames
      allValid = map (all and) checkedGames
      numberOfValidGames = countValidGames allValid gamesIds
  

  print numberOfValidGames
