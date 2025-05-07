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

-- Matrix stores dimensions (rows, cols) and elements in a flat Vector (row-major order)
data Matrix a = Matrix (Int, Int) (Vector a) deriving (Eq, Show)

-- | Returns the number of columns in the matrix.
cols :: Matrix a -> Int
cols (Matrix (_, c) _) = c

-- | Extracts a specific column (1-indexed) as a Vector.
column :: Int -> Matrix a -> Vector a
column c (Matrix (r, cs) vec) =
    let c' = c - 1 -- 0-based index
    in V.backpermute vec $ V.enumFromStepN c' cs r

-- | Flattens the matrix into a single Vector.
flatten :: Matrix a -> Vector a
flatten (Matrix _ vec) = vec

-- | Creates a matrix from a list of lists (rows).
-- Assumes the input is rectangular.
fromList :: [[a]] -> Matrix a
fromList [] = Matrix (0, 0) V.empty
fromList xss@(xs:_) =
    let r = length xss
        c = length xs
        vec = V.fromList $ concat xss
    in Matrix (r, c) vec

-- | Creates a matrix from a string representation.
-- Numbers are separated by spaces, rows by newlines.
fromString :: Read a => String -> Matrix a
fromString s = fromList $ map (map read . words) (lines s)

-- | Changes the shape of the matrix without changing its elements.
-- The total number of elements must remain the same.
reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape newShape@(nr, nc) (Matrix (r, c) vec)
    | nr * nc == r * c = Matrix newShape vec
    | otherwise        = error "New shape must have the same number of elements" -- Or handle error differently if needed

-- | Extracts a specific row (1-indexed) as a Vector.
row :: Int -> Matrix a -> Vector a
row r (Matrix (_, c) vec) =
    let r' = r - 1 -- 0-based index
        startIndex = r' * c
    in V.slice startIndex c vec

-- | Returns the number of rows in the matrix.
rows :: Matrix a -> Int
rows (Matrix (r, _) _) = r

-- | Returns the shape (rows, cols) of the matrix.
shape :: Matrix a -> (Int, Int)
shape (Matrix dims _) = dims

-- | Transposes the matrix (swaps rows and columns).
transpose :: Matrix a -> Matrix a
transpose matrix@(Matrix (r, c) _) =
    let transposedData = V.concat [column i matrix | i <- [1..c]]
    in Matrix (c, r) transposedData
