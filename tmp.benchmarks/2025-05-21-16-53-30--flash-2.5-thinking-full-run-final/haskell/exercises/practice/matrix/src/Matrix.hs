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

import Data.Vector (Vector, (!), toList, concat, generate) -- Removed 'fromList' from here
import qualified Data.Vector as V

-- Define the Matrix data type
-- It stores the number of rows, number of columns, and the matrix data
-- as a Vector of Vectors.
data Matrix a = Matrix { matrixRows :: Int
                       , matrixCols :: Int
                       , matrixData :: Vector (Vector a)
                       } deriving (Eq, Show)

-- Helper to convert a list of lists to a Vector of Vectors
listToVectorOfVectors :: [[a]] -> Vector (Vector a)
listToVectorOfVectors = V.fromList . map V.fromList

-- fromList :: [[a]] -> Matrix a
-- Creates a Matrix from a list of lists.
-- Assumes the input list of lists is rectangular.
fromList :: [[a]] -> Matrix a
fromList xss =
    let numRows = length xss
        -- Determine number of columns from the first row, or 0 if no rows.
        numCols = if numRows > 0 then length (head xss) else 0
        vecData = listToVectorOfVectors xss
    in Matrix numRows numCols vecData

-- fromString :: Read a => String -> Matrix a
-- Creates a Matrix by parsing a string.
-- Each line represents a row, and numbers are space-separated.
fromString :: Read a => String -> Matrix a
fromString s =
    let stringRows = lines s
        -- Parse each line into a list of numbers
        parsedRows = map (map read . words) stringRows
    in fromList parsedRows

-- rows :: Matrix a -> Int
-- Returns the number of rows in the matrix.
rows :: Matrix a -> Int
rows = matrixRows

-- cols :: Matrix a -> Int
-- Returns the number of columns in the matrix.
cols = matrixCols

-- shape :: Matrix a -> (Int, Int)
-- Returns the dimensions of the matrix as a (rows, columns) tuple.
shape :: Matrix a -> (Int, Int)
shape m = (matrixRows m, matrixCols m)

-- row :: Int -> Matrix a -> Vector a
-- Returns the specified row (0-based index) as a Vector.
row :: Int -> Matrix a -> Vector a
row rIdx m = (matrixData m) ! rIdx

-- column :: Int -> Matrix a -> Vector a
-- Returns the specified column (0-based index) as a Vector.
column :: Int -> Matrix a -> Vector a
column cIdx m = V.generate (matrixRows m) (\rIdx -> (matrixData m ! rIdx) ! cIdx)

-- flatten :: Matrix a -> Vector a
-- Flattens the matrix into a single Vector, reading elements row by row.
flatten :: Matrix a -> Vector a
flatten m = V.concat (matrixData m)

-- transpose :: Matrix a -> Matrix a
-- Returns a new matrix that is the transpose of the input matrix.
transpose :: Matrix a -> Matrix a
transpose m =
    let r = matrixRows m
        c = matrixCols m
        oldData = matrixData m
        -- Generate new data by swapping row and column indices
        newData = V.generate c (\colIdx -> V.generate r (\rowIdx -> (oldData ! rowIdx) ! colIdx))
    in Matrix c r newData

-- reshape :: (Int, Int) -> Matrix a -> Matrix a
-- Reshapes the matrix to new dimensions (newRows, newCols).
-- The total number of elements must remain the same.
reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (newRows, newCols) m =
    let oldRows = matrixRows m
        oldCols = matrixCols m
        totalElements = oldRows * oldCols
    in if newRows * newCols /= totalElements
       then error "Cannot reshape to different total number of elements"
       else
           let flatData = flatten m
               -- Generate new data by distributing flattened elements into new dimensions
               newData = V.generate newRows (\rIdx -> V.generate newCols (\cIdx -> flatData ! (rIdx * newCols + cIdx)))
           in Matrix newRows newCols newData
