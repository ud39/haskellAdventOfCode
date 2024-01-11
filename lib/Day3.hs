module Day3 (extractNumberCoordinates, extractSymbolsCoordinates, extractedStarsCoordinates, calcGearRatioValue, calcSumValidNumbers) where

import Data.Char (isDigit)

newtype Coordinates = Coordinates {getCoordinates :: (String, (Int, Int))} deriving (Show)

getCoord :: Coordinates -> (Int, Int)
getCoord = snd . getCoordinates

getNumber :: Coordinates -> Int
getNumber = read . fst . getCoordinates

extractNumberCoordinates :: String -> [Coordinates]
extractNumberCoordinates row =
  let go :: String -> Int -> Int -> [Coordinates] -> [Coordinates]
      go [] _ _ coords = reverse coords
      go (c : cs) rowIdx colIdx coords
        | c == '.' = go cs rowIdx (colIdx + 1) coords
        | isDigit c =
            let (digits, rest) = span isDigit (c : cs)
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
      go (c : cs) rowIdx colIdx coords
        | c == '.' = go cs rowIdx (colIdx + 1) coords
        | not (isDigit c) =
            let coords' = (rowIdx, colIdx)
             in go cs rowIdx (colIdx + 1) (coords' : coords)
        | otherwise = go cs rowIdx (colIdx + 1) coords

      processRow :: String -> Int -> [(Int, Int)]
      processRow newRow rowIdx = go newRow rowIdx 0 []
   in processRow row 0

extractedStarsCoordinates :: String -> [(Int, Int)]
extractedStarsCoordinates row =
  let go :: String -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
      go [] _ _ coords = reverse coords
      go (c : cs) rowIdx colIdx coords
        | c == '*' =
            let coords' = (rowIdx, colIdx)
             in go cs rowIdx (colIdx + 1) (coords' : coords)
        | otherwise = go cs rowIdx (colIdx + 1) coords

      processRow :: String -> Int -> [(Int, Int)]
      processRow newRow rowIdx = go newRow rowIdx 0 []
   in processRow row 0

checkIfNumberAdjacent :: Int -> Coordinates -> [[(Int, Int)]] -> Int
checkIfNumberAdjacent index currentNumber symbol
  | index == 0 && check (getCoord currentNumber) symbolsElementAtIndexPlusOne = getNumber currentNumber
  | index == length symbol - 1 && check (getCoord currentNumber) symbolsElementAtIndexMinusOne = getNumber currentNumber
  | index > 0
      && index < length symbol - 1
      && ( check (getCoord currentNumber) symbolsElementAtIndexMinusOne
             || check (getCoord currentNumber) symbolsElementAtIndex
             || check (getCoord currentNumber) symbolsElementAtIndexPlusOne
         ) =
      getNumber currentNumber
  | otherwise = 0
  where
    symbolsElementAtIndexMinusOne = symbol !! (index - 1)
    symbolsElementAtIndexPlusOne = symbol !! (index + 1)
    symbolsElementAtIndex = symbol !! index

    check :: (Int, Int) -> [(Int, Int)] -> Bool
    check numberCoordinates symbolCoordinates =
      let (numberX, numberY) = numberCoordinates
       in any (\(_, symY) -> abs (numberY - symY) <= 1 || abs (numberX - symY) <= 1) symbolCoordinates

calcGearRatio :: Int -> (Int, Int) -> [[Coordinates]] -> [Int]
calcGearRatio index symbol coordinates
  | index == 0 = check symbol (coordinatesElementAtIndexPlusOne ++ coordinatesElementAtIndex)
  | index == length coordinates - 1 = check symbol (coordinatesElementAtIndexMinusOne ++ coordinatesElementAtIndex)
  | index > 0 && index < length coordinates - 1 = check symbol (coordinatesElementAtIndex ++ coordinatesElementAtIndexMinusOne ++ coordinatesElementAtIndexPlusOne)
  | otherwise = []
  where
    coordinatesElementAtIndexMinusOne = coordinates !! (index - 1)
    coordinatesElementAtIndexPlusOne = coordinates !! (index + 1)
    coordinatesElementAtIndex = coordinates !! index

    check :: (Int, Int) -> [Coordinates] -> [Int]
    check symbolCoordinate numberCoordinates =
      let (_, symbolY) = symbolCoordinate
          validCoordinates = filter (\(Coordinates (_, (coordX, coordY))) -> abs (coordY - symbolY) <= 1 || abs (coordX - symbolY) <= 1) numberCoordinates
       in if length validCoordinates == 2
            then map getNumber validCoordinates
            else []

calcGearRatioValue :: [[Coordinates]] -> [[(Int, Int)]] -> Int
calcGearRatioValue extractedNumbers extractedStars = sum (map product (filter (not . null) gearRatio))
  where
    gearRatio = [calcGearRatio i star extractedNumbers | (i, stars) <- zip [0 ..] extractedStars, star <- stars]

calcSumValidNumbers :: [[Coordinates]] -> [[(Int, Int)]] -> Int
calcSumValidNumbers extractedNumbers extractedSymbols = sum validNumbers
  where
    validNumbers = [checkIfNumberAdjacent i coord extractedSymbols | (i, coordinates) <- zip [0 ..] extractedNumbers, coord <- coordinates]