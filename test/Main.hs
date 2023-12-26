module Main (main) where

import MyLib
import ReadInput
import Control.Arrow (second, (&&&))
import Data.List (intersect)

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

howManyMatches :: [[Int]] -> [[Int]] -> [[Int]]
howManyMatches myNumbers winningNumbers = [ i `intersect` j | (i, j) <- zip myNumbers winningNumbers ]


howManyScratchCards :: [Int] -> [Int]
howManyScratchCards amountOfMatchingNumbers =
    let amountOfCards = replicate (length amountOfMatchingNumbers) 1

        updateListIncrementally :: [Int] -> [Int] -> [Int] -> [Int]
        updateListIncrementally [] updatedList _ = updatedList
        updateListIncrementally (x:xs) updatedList (i:is) =
            let updatedList' = updateAmountOfCards x (updatedList !! (i-1)) i updatedList
            in updateListIncrementally xs updatedList' is 

        indicesList = map (+1) $ zipWith const [0..] amountOfMatchingNumbers
        
    in updateListIncrementally amountOfMatchingNumbers amountOfCards indicesList


updateAmountOfCards :: Int -> Int -> Int -> [Int] -> [Int]
updateAmountOfCards 0 _ _ xs = xs
updateAmountOfCards n c startIndex xs = updateNextN n startIndex xs
  where
    updateNextN :: Int -> Int -> [Int] -> [Int]
    updateNextN 0 _ rest = rest
    updateNextN m i (x:xs')
        | i == 0 = (x + c) : updateNextN (m - 1) i xs'  
        | otherwise = x : updateNextN m (i - 1) xs' 
    updateNextN _ _ [] = []



main :: IO ()
main = do
  scratchCards <- readFileAsLines  "./input/2023/day4.txt"
  let myNumbers = map extractMyNumbers scratchCards
      winningNumbers = map extractWinningNumbers scratchCards
      howMuchIWon = howManyMatches myNumbers winningNumbers
      amountOfMatchingNumbers = map length howMuchIWon
      valueOfWinning = sum [ 2^(n - 1) | n <- filter (/= 0) amountOfMatchingNumbers ]
      updatedList = howManyScratchCards amountOfMatchingNumbers
      updatedListed = updateAmountOfCards 4 2 1 [1, 1, 1, 1, 1, 1]
      sumOfCards = sum updatedList

  

  print scratchCards
  print myNumbers
  print winningNumbers
  print howMuchIWon
  print amountOfMatchingNumbers
  print updatedList
  print updatedListed
  print sumOfCards
