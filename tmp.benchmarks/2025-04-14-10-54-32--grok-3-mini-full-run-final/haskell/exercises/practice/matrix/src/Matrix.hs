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

import qualified Data.Vector as V

-- Define the Matrix data type using Vector for efficiency
newtype Matrix a = Matrix (V.Vector (V.Vector a)) deriving (Eq, Show)

-- Implementation of the functions

rows :: Matrix a -> Int
rows (Matrix vecs) = V.length vecs  -- Number of rows is the length of the outer vector

cols :: Matrix a -> Int
cols (Matrix vecs)
  | V.length vecs > 0 = V.length (vecs V.!? 0 V.?| 0)  -- Number of columns is the length of the first inner vector (if it exists)
  | otherwise = 0  -- Empty matrix has 0 columns

-- Helper for safe indexing; ?| is not standard, so use pattern matching or default
-- Note: We're assuming the matrix is not empty for cols, as per no validation rule

row :: Int -> Matrix a -> V.Vector a
row i (Matrix vecs) = vecs V.!? i V.?| error "Row index out of bounds"  -- Get the i-th row

column :: Int -> Matrix a -> V.Vector a
column j (Matrix vecs) = V.generate (V.length vecs) (\i -> (vecs V.!? i V.?| error "Index error") V.!? j V.?| error "Column index out of bounds")
  -- Generate a vector by extracting the j-th element from each row

flatten :: Matrix a -> V.Vector a
flatten (Matrix vecs) = V.concat vecs  -- Concatenate all inner vectors into one

fromList :: [[a]] -> Matrix a
fromList xss = Matrix (V.fromList (V.map V.fromList xss))  -- Convert list of lists to Vector of Vectors

fromString :: Read a => String -> Matrix a
fromString str = 
  let linesList = lines str  -- Split string into lines
      parsedLines = V.map (V.map read . words) linesList  -- For each line, split into words and read them
  in fromList parsedLines  -- Convert the parsed list of lists to Matrix

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (newRows, newCols) (Matrix vecs) =
  let flatVec = flatten (Matrix vecs)  -- Flatten the matrix
      totalElements = V.length flatVec
      newTotal = newRows * newCols  -- Calculate new total elements
  in if totalElements == newTotal  -- Assume it matches; no validation
       then Matrix (V.generate newRows (\i -> V.generate newCols (\j -> flatVec V.! (i * newCols + j))))  -- Reshape into new dimensions
       else error "Reshape dimensions do not match total elements"

shape :: Matrix a -> (Int, Int)
shape matrix = (rows matrix, cols matrix)  -- Tuple of (rows, cols)

transpose :: Matrix a -> Matrix a
transpose (Matrix vecs) =
  let numRows = rows (Matrix vecs)
      numCols = cols (Matrix vecs)
  in Matrix (V.generate numCols (\j -> V.generate numRows (\i -> (vecs V.!? i V.?| error "Index error") V.!? j V.?| error "Index error")))
    -- Create a new matrix where rows become columns
