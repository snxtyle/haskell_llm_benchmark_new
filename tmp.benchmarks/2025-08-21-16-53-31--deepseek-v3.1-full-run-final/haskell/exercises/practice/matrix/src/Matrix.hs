module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , transpose
    ) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.List as L

-- Represent matrix as (rows, columns, flattened data)
data Matrix a = Matrix Int Int (Vector a) deriving (Eq, Show)

cols :: Matrix a -> Int
cols (Matrix _ c _) = c

column :: Int -> Matrix a -> Vector a
column colIndex (Matrix r c vec)
    | colIndex < 0 || colIndex >= c = error "Column index out of bounds"
    | otherwise = V.generate r (\row -> vec V.! (row * c + colIndex))

flatten :: Matrix a -> Vector a
flatten (Matrix _ _ vec) = vec

fromList :: [[a]] -> Matrix a
fromList [] = Matrix 0 0 V.empty
fromList ([]:_) = Matrix 0 0 V.empty
fromList xss = Matrix rows cols (V.fromList (concat xss))
    where
        rows = length xss
        cols = length (head xss)

fromString :: Read a => String -> Matrix a
fromString xs = fromList (parseMatrix xs)
    where
        parseMatrix :: Read a => String -> [[a]]
        parseMatrix = map (map read . words) . lines

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (newRows, newCols) (Matrix _ _ vec)
    | newRows * newCols /= V.length vec = error "New dimensions don't match element count"
    | otherwise = Matrix newRows newCols vec

row :: Int -> Matrix a -> Vector a
row rowIndex (Matrix r c vec)
    | rowIndex < 0 || rowIndex >= r = error "Row index out of bounds"
    | otherwise = V.slice (rowIndex * c) c vec

rows :: Matrix a -> Int
rows (Matrix r _ _) = r

shape :: Matrix a -> (Int, Int)
shape (Matrix r c _) = (r, c)

transpose :: Matrix a -> Matrix a
transpose (Matrix r c vec) = Matrix c r (V.fromList (concat (L.transpose (chunksOf c (V.toList vec)))))
    where
        chunksOf :: Int -> [a] -> [[a]]
        chunksOf _ [] = []
        chunksOf n xs = take n xs : chunksOf n (drop n xs)
