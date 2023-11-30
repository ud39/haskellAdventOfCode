module ReadInput (readFileAsString, readFileAsLines, readFileAsArrays) where

import Data.Maybe
import Text.Read (readMaybe)

readFileAsString :: FilePath -> IO String
readFileAsString = readFile

readFileAsLines :: FilePath -> IO [String]
readFileAsLines path = lines <$> readFile path

readFileAsArrays :: FilePath -> IO [Int]
readFileAsArrays path = do
  contents <- readFileAsString path
  pure $ mapMaybe readMaybeToInt (lines contents)

readMaybeToInt :: String -> Maybe Int
readMaybeToInt str = fromMaybe Nothing (readMaybe str)
