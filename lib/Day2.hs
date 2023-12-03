module Day2 (parseGame, extractInitialCubes) where

import Data.Char (isDigit)
import Data.List (dropWhileEnd)
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

extractInitialCubes :: String -> [String]
extractInitialCubes initialCubes = map (dropWhile (not . isDigit) . trimBoth) $ takeWhile (/= 'c') <$> splitOn ", " initialCubes

trimBoth :: String -> String
trimBoth = dropWhileEnd (== ' ') . dropWhile (== ' ')

-- validGames :: [String] -> Int
-- validGames game = do