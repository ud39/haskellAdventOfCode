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
  numbersList <- readFileAsLines "input/2023/day1.txt"
  let solutions = calcCalibrationValue numbersList
  print solutions