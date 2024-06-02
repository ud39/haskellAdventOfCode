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
  scratchCards <- readFileAsLines  "./input/2023/day4.txt"
  let 
      result_part_1 = getTotalAmountOfPoints scratchCards
      result_part_2 = calculateSumOfCards scratchCards

  print result_part_1
  print result_part_2