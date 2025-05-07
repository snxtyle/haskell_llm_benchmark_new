module Matrix
    ( Matrix(..) -- Exporting the data constructor (Matrix) along with the type (Matrix)
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

import qualified Data.Vector as V

-- | A matrix represented as a vector of rows, where each row is also a vector.
data Matrix a = Matrix (V.Vector (V.Vector a)) deriving (Eq, Show)

-- | Creates a matrix from a list of lists.
-- Each inner list represents a row.
fromList :: [[a]] -> Matrix a
fromList xss = Matrix (V.fromList (map V.fromList xss))

-- | Returns the number of rows in the matrix.
rows :: Matrix a -> Int
rows (Matrix vs) = V.length vs

-- | Returns the number of columns in the matrix.
-- Assumes the matrix is rectangular. If there are no rows, there are no columns.
-- Otherwise, it's the length of the first row.
cols :: Matrix a -> Int
cols (Matrix vs)
    | V.null vs = 0
    | otherwise = V.length (V.head vs)

-- | Returns the dimensions of the matrix as a tuple (number of rows, number of columns).
shape :: Matrix a -> (Int, Int)
shape m = (rows m, cols m)

-- | Returns the specified row (1-indexed) of the matrix as a vector.
-- Example: `row 1 myMatrix` gets the first row.
-- Behavior for out-of-bounds index is determined by `V.!` (typically an error),
-- aligning with "Let it fail".
row :: Int -> Matrix a -> V.Vector a
row r (Matrix vs) = vs V.! (r - 1)

-- | Returns the specified column (1-indexed) of the matrix as a vector.
-- Example: `column 1 myMatrix` gets the first column.
-- Behavior for out-of-bounds index is determined by `V.!` (typically an error),
-- aligning with "Let it fail".
column :: Int -> Matrix a -> V.Vector a
column c (Matrix vs) = V.map (\rowVec -> rowVec V.! (c - 1)) vs

-- | Parses a string representation into a matrix.
-- The string should have rows separated by newlines,
-- and elements within rows separated by spaces.
-- Elements are parsed using `read`.
-- Example: `fromString "1 2\n3 4"`
fromString :: Read a => String -> Matrix a
fromString s = fromList (map (map read . words) (lines s))

-- | Flattens the matrix into a single vector, concatenating rows.
flatten :: Matrix a -> V.Vector a
flatten (Matrix vs) = V.concat (V.toList vs) -- V.concat expects a List of Vectors

-- | Transposes the matrix (swaps rows and columns).
-- If the matrix is M_rc (r rows, c columns), its transpose is M_cr (c rows, r columns).
transpose :: Matrix a -> Matrix a
transpose m =
    let
        r = rows m
        c = cols m
    in if r == 0 || c == 0
        -- If either dimension is 0, the transpose is an empty matrix.
        -- `fromList []` creates `Matrix V.empty`, representing a 0x0 matrix.
        then fromList []
        else
             -- The i-th column of the original matrix becomes the i-th row of the new matrix.
             fromList [ V.toList (column i m) | i <- [1..c] ]

-- | Reshapes the matrix to the given new dimensions (newRows, newCols).
-- Elements are taken row-by-row from the flattened original matrix.
-- "Let it fail" (e.g., via `V.slice` errors) if `newRows * newCols`
-- does not match the total number of elements in the original matrix,
-- or if dimensions are invalid for `V.slice`.
reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (newRows, newCols) m
    | newRows < 0 || newCols < 0 = error "reshape: dimensions cannot be negative"
    | otherwise =
        let
            flatVector = flatten m
            -- For each new row `idx` (0-indexed for list comprehension),
            -- take a slice of `newCols` elements from `flatVector`
            -- starting at `idx * newCols`.
            buildRow idx = V.slice (idx * newCols) newCols flatVector
            
            -- Create `newRows` such rows.
            listOfRowVectors = [buildRow i | i <- [0 .. newRows - 1]]
        in Matrix (V.fromList listOfRowVectors)
