module Main (main) where

import MyLib
import ReadInput
import Data.Char (isDigit, digitToInt)

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


newtype Coordinates = Coordinates (String, (Int, Int)) deriving Show

extractNumberCoordinates :: String -> [Coordinates]
extractNumberCoordinates row =
  let go :: String -> Int -> Int -> [Coordinates] -> [Coordinates]
      go [] _ _ coords = reverse coords
      go (c:cs) rowIdx colIdx coords
        | c == '.' = go cs rowIdx (colIdx + 1) coords
        | isDigit c =
            let (digits, rest) = span isDigit (c:cs)
                end = colIdx + length digits - 1
                coords' = Coordinates (digits, (colIdx, end)) : coords
            in go rest rowIdx (end + 1) coords'
        | otherwise = go cs rowIdx (colIdx + 1) coords

      processRow :: String -> Int -> [Coordinates]
      processRow newRow rowIdx = go newRow rowIdx 0 []

  in processRow row 0



extractSymbolsCoordinates :: String -> [(Int, Int)]
extractSymbolsCoordinates row =
  let go :: String -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
      go [] _ _ coords = reverse coords
      go (c:cs) rowIdx colIdx coords
        | c == '.' = go cs rowIdx (colIdx + 1) coords
        | not (isDigit c) =
            let coords' = (rowIdx, colIdx)
            in go cs rowIdx (colIdx + 1) (coords' : coords)
        | otherwise = go cs rowIdx (colIdx + 1) coords

      processRow :: String -> Int -> [(Int, Int)]
      processRow newRow rowIdx = go newRow rowIdx 0 []

  in processRow row 0


main :: IO ()
main = do
  schematic <- readFileAsLines "./input/2023/day3.txt"
  let exampleInput = ["467..114..","...*......","..35..633.","......#...","617*......",".....+.58.","..592.....","......755.","...$.*....",".664.598.."]
      extractedNumbers = map extractNumberCoordinates exampleInput
      extractedSymbols = map extractSymbolsCoordinates exampleInput
  print extractedNumbers
  print extractedSymbols

