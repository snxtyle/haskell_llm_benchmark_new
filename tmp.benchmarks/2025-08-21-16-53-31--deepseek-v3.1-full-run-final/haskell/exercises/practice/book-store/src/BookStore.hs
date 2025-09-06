module BookStore (total, Book(..)) where

import Data.List (sort, sortBy)
import Data.Ord (comparing)

data Book = First | Second | Third | Fourth | Fifth
    deriving (Eq, Show, Enum, Bounded, Ord)

bookPrice :: Int
bookPrice = 800

discounts :: [Double]
discounts = [0, 0, 0.05, 0.10, 0.20, 0.25]

total :: [Book] -> Int
total basket = 
    let counts = map (\b -> length $ filter (== b) basket) [First .. Fifth]
        sortedCounts = sortBy (flip compare) counts
    in minTotal sortedCounts

minTotal :: [Int] -> Int
minTotal counts
    | all (== 0) counts = 0
    | otherwise = minimum possibleCosts
  where
    nonZeroCount = length . filter (> 0)
    maxGroupSize = min 5 (nonZeroCount counts)
    
    possibleCosts = [ cost groupSize + minTotal (removeGroup groupSize counts) 
                    | groupSize <- [1..maxGroupSize] ]
    
    cost :: Int -> Int
    cost n = round $ fromIntegral (n * bookPrice) * (1 - discounts !! n)
    
    removeGroup :: Int -> [Int] -> [Int]
    removeGroup n counts = 
        let sorted = sortBy (flip compare) counts
            (toRemove, rest) = splitAt n sorted
            remaining = zipWith (-) toRemove (replicate n 1) ++ rest
        in sortBy (flip compare) $ filter (> 0) remaining
