module Day4 ( calculateSumOfCards, getTotalAmountOfPoints )  where

import Data.List ( intersect )

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


calculateSumOfCards :: [String] -> Int
calculateSumOfCards scratchCardsNumbers =
    let
        -- Extract numbers 
        myNumbers = map extractMyNumbers scratchCardsNumbers
        winningNumbers = map extractWinningNumbers scratchCardsNumbers
        -- Calculate matches
        howMuchIWon = howManyMatches myNumbers winningNumbers
        -- Calculate the lengths of matched numbers
        amountOfMatchingNumbers = map length howMuchIWon
        -- Calculate value of winning numbers
        valueOfWinning = sum [ 2^(n - 1) | n <- filter (/= 0) amountOfMatchingNumbers ]
        -- Update list based on matches 
        updatedList = howManyScratchCards amountOfMatchingNumbers
        -- Sum up the updated list
        sumOfCards = sum updatedList
    in sumOfCards

getTotalAmountOfPoints :: [String] -> Int
getTotalAmountOfPoints scratchCards = result
    where
        myNumbers = map extractMyNumbers scratchCards
        winningNumbers = map extractWinningNumbers scratchCards
        howMuchIWon = [  i `intersect` j | (i, j) <- zip myNumbers winningNumbers]
        amountOfMatchingNumbers = map length howMuchIWon
        result = sum [ 2^(n-1) | n <- filter (/= 0) amountOfMatchingNumbers]
        
