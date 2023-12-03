module Main (main) where

import MyLib
import ReadInput
import Test.Tasty
import Test.Tasty.HUnit

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

main :: IO ()
main = do
  let initialCubesString = "12 red cubes, 13 green cubes, and 14 blue cubes"
  let initialCubes = extractInitialCubes initialCubesString
  games <- readFileAsLines "input/2023/day2.txt"
  let parsedGames = map parseGame games
  print parsedGames
  print initialCubes