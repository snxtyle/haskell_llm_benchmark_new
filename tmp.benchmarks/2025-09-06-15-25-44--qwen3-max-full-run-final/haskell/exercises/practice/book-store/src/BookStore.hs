module BookStore (total, Book(..)) where

import Data.List (group, sort, nub)

data Book = First | Second | Third | Fourth | Fifth deriving (Eq, Ord, Enum, Bounded)

-- Calculate the total price in cents for a basket of books
total :: [Book] -> Int
total [] = 0
total basket = minimum [calculatePrice groups | groups <- generateAllGroupings basket]
  where
    -- Generate all possible ways to group the books
    generateAllGroupings :: [Book] -> [[[Book]]]
    generateAllGroupings [] = [[]]
    generateAllGroupings books = 
        let uniqueBooks = nub books
            maxGroupSize = min 5 (length uniqueBooks)
            -- Generate groups of different sizes
            allGroups = [g | size <- [1..maxGroupSize], g <- generateGroupsOfSize books size]
        in concatMap (\firstGroup -> 
            let remaining = removeFromList books firstGroup
                restGroupings = generateAllGroupings remaining
            in map (firstGroup:) restGroupings
        ) allGroups
    
    -- Generate all possible groups of a specific size
    generateGroupsOfSize :: [Book] -> Int -> [[Book]]
    generateGroupsOfSize books size = 
        let uniqueBooks = nub books
        in if length uniqueBooks < size
           then []
           else generateCombinations uniqueBooks size
    
    -- Generate all combinations of n elements from a list
    generateCombinations :: [Book] -> Int -> [[Book]]
    generateCombinations _ 0 = [[]]
    generateCombinations [] _ = []
    generateCombinations (x:xs) n = 
        map (x:) (generateCombinations xs (n-1)) ++ generateCombinations xs n
    
    -- Remove elements of the first list from the second list (one occurrence each)
    removeFromList :: [Book] -> [Book] -> [Book]
    removeFromList original [] = original
    removeFromList [] _ = []
    removeFromList original (x:xs) = 
        case break (== x) original of
            (before, []) -> removeFromList original xs  -- x not found
            (before, _:after) -> removeFromList (before ++ after) xs
    
    -- Calculate price for a specific grouping
    calculatePrice :: [[Book]] -> Int
    calculatePrice groups = sum $ map groupPrice groups
    
    -- Calculate price for one group
    groupPrice :: [Book] -> Int
    groupPrice [] = 0
    groupPrice books = 
        let uniqueCount = length $ nub books
            bookCount = length books
            basePrice = 800 * bookCount
        in case uniqueCount of
            1 -> basePrice
            2 -> (basePrice * 95) `div` 100
            3 -> (basePrice * 90) `div` 100
            4 -> (basePrice * 80) `div` 100
            5 -> (basePrice * 75) `div` 100
            _ -> basePrice
