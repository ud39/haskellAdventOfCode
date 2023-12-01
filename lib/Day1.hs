module Day1 (extractNumbers) where

import Data.Char (isDigit)

-- Helper function to get the sum of the first and last digit forming a two-digit number
sumFirstLastDigit :: Int -> Int
sumFirstLastDigit n = read [head digits, last digits]
  where
    digits = show n

-- extractNumbers function
extractNumbers :: [String] -> [Int]
extractNumbers = map (sumFirstLastDigit . read . filter isDigit)
