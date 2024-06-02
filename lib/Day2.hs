module Day2 (getValidAmountOfGames, getMinCubeNeeded) where

import Data.List (dropWhileEnd, isInfixOf)
import Data.List.Split (splitOn)

parseGame :: String -> [[String]]
parseGame input =
  [extractGame input]
    : map
      (map trimBoth . splitOn ", ")
      (splitOn "; " $ tail $ dropWhile (/= ':') input)
  where
    extractGame :: String -> String
    extractGame str = case splitOn ": " str of
      [gamePart, _] -> gamePart
      _ -> ""

trimBoth :: String -> String
trimBoth = dropWhileEnd (== ' ') . dropWhile (== ' ')

-- Function to get the count of a specific color from available counts
getCount :: String -> [String] -> Int
getCount color counts =
  case filter (\s -> color `isInfixOf` s) counts of
    [] -> 0
    (x:_) -> read . head . words $ x

getIndexes :: [[[String]]] -> [Int]
getIndexes xs = map (+1) [0..(length xs - 1)]

compareCounts :: [String] -> [String] -> [Bool]
compareCounts availableCounts play =
  [ getCount color availableCounts >= getCount color play | color <- colors ]
  where
    colors = map (dropWhile (== ' ') . dropWhile (/= ' ')) availableCounts


countValidGames :: [Bool] -> [Int] -> Int
countValidGames validGames indexes = sum $ zipWith (\valid idx -> if valid then idx else 0) validGames indexes

calcMinCubesNeed :: [String] -> [[String]] -> [Int]
calcMinCubesNeed availableCounts games =
    map (\color -> maximum [getCount color game | game <- games]) colors
    where
        colors = map (dropWhile (== ' ') . dropWhile (/= ' ')) availableCounts

getValidAmountOfGames :: [String] -> [String] -> Int
getValidAmountOfGames games availableBalls = result
  where
    parsedGames = map parseGame games
    playedGames = map tail parsedGames
    gamesIds = getIndexes parsedGames

    checkedGames = map (map (compareCounts availableBalls)) playedGames
    allValid = map (all and) checkedGames
    result = countValidGames allValid gamesIds


getMinCubeNeeded :: [String] -> [String] -> Int
getMinCubeNeeded games availableCounts = result
  where
    parsedGames = map parseGame games
    playedGames = map tail parsedGames
    result = sum (map (product . calcMinCubesNeed availableCounts) playedGames)