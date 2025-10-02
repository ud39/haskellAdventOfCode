module Day5 (calculatePartOne) where

import Data.Char (isAlpha, isAlphaNum, isDigit)

------- Part one
getInitialSeed :: String -> [Int]
getInitialSeed seeds = map read (words (dropWhile (/= ' ') seeds))

getNextMapping :: [String] -> [(String, [Int])]
getNextMapping [] = []
getNextMapping (x : xs)
  | any isAlpha x =
      let (group, rest) = span (all (\c -> c == ' ' || isDigit c)) xs
          workflow = takeWhile (/= ' ') x
          mapRange = concatMap (map read . words) group
       in (workflow, mapRange) : getNextMapping rest
  | otherwise = getNextMapping xs

transformToNextValue :: Int -> [Int] -> Int
transformToNextValue source [] = source
transformToNextValue source range =
  let currentRange = take 3 range
      restRange = drop 3 range
      upperBound = head (tail currentRange) + last currentRange
      lowerBound = head (tail currentRange)
      checkRange r
        | source >= lowerBound && source <= upperBound = head r + abs (source - head (tail currentRange))
        | otherwise = transformToNextValue source restRange
   in checkRange currentRange

getNextStep :: [Int] -> [[Int]] -> [Int]
getNextStep xs plan =
  map (\x -> foldl transformToNextValue x plan) xs

calculatePartOne :: [String] -> Int
calculatePartOne input =
  let
    seeds = getInitialSeed $ head input
    plan = filter (any isAlphaNum) (tail input)
    mappings = getNextMapping plan
    range = map snd mappings
    test = getNextStep seeds range
    solution = minimum test
   in
    solution

------- Part two
transformToRange :: [Int] -> [(Int, Int)]
transformToRange [] = []
transformToRange (x : y : xs) = (x, y) : transformToRange xs
transformToRange _ = error "Odd numbers of error"
