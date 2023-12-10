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


extractWinningNumbers :: String -> [Int]
extractWinningNumbers scratchCard = 
    map read $ words winningNumbers
    where
        winningNumbers = drop 1 $ dropWhile (/= '|') scratchCard

extractMyNumbers :: String -> [Int]
extractMyNumbers scratchCard = 
    map read $ words myNumbers
    where 
        myNumbers = drop 1 $ takeWhile (/= '|') $ dropWhile (/= ':') scratchCard
        


main :: IO ()
main = do
  scratchCards <- readFileAsLines  "./input/2023/day4.txt"
  let myNumbers = map extractMyNumbers scratchCards
      winningNumbers = map extractWinningNumbers scratchCards

  

  print scratchCards
  print myNumbers
  print winningNumbers
