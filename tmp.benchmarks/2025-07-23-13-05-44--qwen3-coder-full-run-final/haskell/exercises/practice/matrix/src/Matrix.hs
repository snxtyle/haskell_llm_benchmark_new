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

import Data.Vector (Vector, fromList, toList, (!))
import qualified Data.Vector as V

data Matrix a = Matrix { mData :: Vector (Vector a) }
    deriving (Eq, Show)

-- | Get the number of columns in a matrix
cols :: Matrix a -> Int
cols (Matrix rowsVec) 
    | V.null rowsVec = 0
    | otherwise = V.length (rowsVec V.! 0)

-- | Get a specific column from a matrix
column :: Int -> Matrix a -> Vector a
column colIndex (Matrix rowsVec) = V.map (V.! colIndex) rowsVec

-- | Flatten a matrix into a vector
flatten :: Matrix a -> Vector a
flatten (Matrix rowsVec) = V.concat (V.toList rowsVec)

-- | Create a matrix from a list of lists
fromList :: [[a]] -> Matrix a
fromList xss = Matrix (V.fromList (map V.fromList xss))

-- | Create a matrix from a string representation
fromString :: Read a => String -> Matrix a
fromString str = Matrix.fromList [[read num | num <- words line] | line <- lines str]

-- | Reshape a matrix to new dimensions
reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (newRows, newCols) matrix = 
    let flat = flatten matrix
        grouped = groupBy newCols (V.toList flat)
    in Matrix.fromList grouped
  where
    groupBy :: Int -> [a] -> [[a]]
    groupBy _ [] = []
    groupBy n xs = take n xs : groupBy n (drop n xs)

-- | Get a specific row from a matrix
row :: Int -> Matrix a -> Vector a
row rowIndex (Matrix rowsVec) = rowsVec V.! rowIndex

-- | Get the number of rows in a matrix
rows :: Matrix a -> Int
rows (Matrix rowsVec) = V.length rowsVec

-- | Get the shape (rows, cols) of a matrix
shape :: Matrix a -> (Int, Int)
shape matrix = (rows matrix, cols matrix)

-- | Transpose a matrix
transpose :: Matrix a -> Matrix a
transpose matrix = 
    let (r, c) = shape matrix
        colsList = [V.toList (column i matrix) | i <- [0..c-1]]
    in Matrix.fromList colsList
