{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
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

extractFarmingPlan :: String -> [String]
extractFarmingPlan farmingplan = result
  where
    beginState = takeWhile (/= '-') farmingplan

    extractEndState :: Eq a => a -> Int -> [a] -> [a]
    extractEndState _ 0 xs = xs  -- Return the rest of the list when count reaches 0
    extractEndState _ _ [] = []  -- Handle the case when the list is empty
    extractEndState target count (x:xs)
     | x == target = extractEndState target (count - 1) xs  -- Decrement count if target is found
     | otherwise   = extractEndState target count xs  -- Continue processing the rest of the list


    endState = takeWhile (/= ' ') (extractEndState '-' 2 (dropWhile (/= '-') farmingplan)) 
    result = if endState == "" then [beginState] else [beginState, endState]


checkRange :: Int -> Int -> Int -> Int -> Int
checkRange destinationBegin sourceBegin range currentValue = result
  where
    result = case currentValue of
      _ | currentValue + range > currentValue && currentValue >= sourceBegin -> destinationBegin + (currentValue - sourceBegin)
        | otherwise -> currentValue


main :: IO ()
main = do

  input <- readFileAsLines  "./input/2023/day5.txt"
  let 
    cleanup = filter (/= "") input
    seeds = head cleanup
    plan = tail cleanup
    steps = map extractFarmingPlan plan

    test = checkRange 52 50 48 79
  
  -- print cleanup
  -- print seeds
  -- print plan
  -- print steps
  print test
  {-
  let 
      result_part_1 = getTotalAmountOfPoints input
      result_part_2 = calculateSumOfCards input

  print result_part_1
  print result_part_2
  -}