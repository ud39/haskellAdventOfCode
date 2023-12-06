module Day1 (extractNumbers, calcCalibrationValue) where

import Data.Char (intToDigit, isDigit)
import Data.List (find, isPrefixOf, tails)

-- Helper function to get the sum of the first and last digit forming a two-digit number
sumFirstLastDigit :: Int -> Int
sumFirstLastDigit n = read [head digits, last digits]
  where
    digits = show n

-- extractNumbers function
extractNumbers :: [String] -> [Int]
extractNumbers = map (sumFirstLastDigit . read . filter isDigit)

myDigits :: [(String, Int)]
myDigits =
  [ ("one", 1),
    ("two", 2),
    ("three", 3),
    ("four", 4),
    ("five", 5),
    ("six", 6),
    ("seven", 7),
    ("eight", 8),
    ("nine", 9)
  ]
    <> [(pure (intToDigit x), x) | x <- [1 .. 9]]

stringToDigits :: String -> [Int]
stringToDigits input =
  concatMap findMatchingDigits (tails input)
  where
    findMatchingDigits :: String -> [Int]
    findMatchingDigits s =
      case find (\(representation, _) -> representation `isPrefixOf` s) myDigits of
        Just (_, value) -> [value]
        Nothing -> []

calcCalibrationValue :: [String] -> Int
calcCalibrationValue numbersList =
  let firstDigits = map (head . stringToDigits) numbersList
      lastDigits = map (last . stringToDigits) numbersList
      solutions = zipWith (\firstDigit lastDigit -> read (show firstDigit ++ show lastDigit) :: Int) firstDigits lastDigits
   in sum solutions
