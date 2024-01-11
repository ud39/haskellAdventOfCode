module Main (main) where

import Data.Char (isDigit)
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
  schematic <- readFileAsLines "./input/2023/day3.txt"
  let extractedNumbers = map extractNumberCoordinates schematic
      extractedSymbols = map extractSymbolsCoordinates schematic
      extractedStars = map extractedStarsCoordinates schematic
      gearRatioValue = calcGearRatioValue extractedNumbers extractedStars
      sumOfValidNumbers = calcSumValidNumbers extractedNumbers extractedSymbols

  print gearRatioValue
  print sumOfValidNumbers
