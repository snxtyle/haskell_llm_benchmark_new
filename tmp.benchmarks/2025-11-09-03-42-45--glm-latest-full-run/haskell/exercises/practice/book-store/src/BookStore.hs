module BookStore (total, Book(..)) where

import Data.List (tails)
import Data.Array (Array, accumArray, (!))

data Book = First | Second | Third | Fourth | Fifth deriving (Eq, Ord, Show, Enum)

-- | Calculates the total price of a basket of books with the optimal discount.
--   The price is returned in cents.
total :: [Book] -> Int
total basket = minPrice (toCounts basket)
  where
    -- | Converts a list of books into a 5-tuple of counts for each book.
    toCounts :: [Book] -> (Int, Int, Int, Int, Int)
    toCounts b = let
        counts = accumArray (+) 0 (fromEnum First, fromEnum Fifth) [(fromEnum book, 1) | book <- b]
        in (counts ! 0, counts ! 1, counts ! 2, counts ! 3, counts ! 4)

    -- | Recursively searches for the minimum price for a given basket configuration.
    minPrice :: (Int, Int, Int, Int, Int) -> Int
    minPrice counts@(c1, c2, c3, c4, c5)
      | totalBooks == 0 = 0
      | otherwise = minimum $ map tryGroup [1..numDifferentBooks]
      where
        totalBooks = c1 + c2 + c3 + c4 + c5
        countsList = [c1, c2, c3, c4, c5]
        presentBooks = map fst $ filter ((>0) . snd) $ zip [0..] countsList
        numDifferentBooks = length presentBooks

        -- | Tries to form a group of size k and calculates the resulting total price.
        tryGroup :: Int -> Int
        tryGroup k = minimum $ map (tryCombination k) (combinations k presentBooks)

        -- | Calculates the price for a specific combination of books forming a group.
        tryCombination :: Int -> [Int] -> Int
        tryCombination k indices = groupPrice k + minPrice (newCounts indices)

        -- | Creates a new counts tuple after removing one book for each index in the combination.
        newCounts :: [Int] -> (Int, Int, Int, Int, Int)
        newCounts indices = let
            dec i c = if i `elem` indices then c - 1 else c
            in (dec 0 c1, dec 1 c2, dec 2 c3, dec 3 c4, dec 4 c5)

    -- | Calculates the price of a single group of k books in cents.
    groupPrice :: Int -> Int
    groupPrice k = k * 800 * (100 - discount k) `div` 100

    -- | Returns the discount percentage for a group of k books.
    discount :: Int -> Int
    discount 5 = 25
    discount 4 = 20
    discount 3 = 10
    discount 2 = 5
    discount _ = 0

    -- | Generates all k-length combinations of a list.
    combinations :: Int -> [a] -> [[a]]
    combinations 0 _      = [[]]
    combinations n xs
      | n < 0     = []
      | n > length xs = []
      | otherwise = [x:ys | x:xs' <- tails xs, ys <- combinations (n-1) xs']
