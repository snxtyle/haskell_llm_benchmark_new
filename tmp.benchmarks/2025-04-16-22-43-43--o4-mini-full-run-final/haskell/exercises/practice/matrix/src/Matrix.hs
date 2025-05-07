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

-- A row-major matrix with given number of rows and columns,
-- storing elements in a flat Vector.
data Matrix a = Matrix Int Int (Vector a)
    deriving (Eq, Show)

-- Plural: all columns as a list of Vectors (1-based indexing under the hood)
cols :: Matrix a -> [Vector a]
cols m@(Matrix _ c _) = [ column j m | j <- [1..c] ]

-- Plural: all rows as a list of Vectors (1-based indexing under the hood)
rows :: Matrix a -> [Vector a]
rows (Matrix r c v) = [ V.slice (i * c) c v | i <- [0..r-1] ]

-- (rows, cols)
shape :: Matrix a -> (Int, Int)
shape (Matrix r c _) = (r, c)

-- Flatten to the underlying vector
flatten :: Matrix a -> Vector a
flatten (Matrix _ _ v) = v

-- Build from a list of lists (assumes rectangular)
fromList :: [[a]] -> Matrix a
fromList xss =
    let r = length xss
        c = if null xss then 0 else length (head xss)
        v = V.fromList (concat xss)
    in Matrix r c v

-- Parse from a string of whitespace-separated numbers and newlines
fromString :: Read a => String -> Matrix a
fromString xs = fromList (map (map read . words) (lines xs))

-- Change dimensions (must match total element count)
reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r', c') (Matrix r c v)
    | r * c /= r' * c' = error "reshape: incompatible dimensions"
    | otherwise        = Matrix r' c' v

-- Extract a single row (1-based)
row :: Int -> Matrix a -> Vector a
row i (Matrix r c v)
    | i < 1 || i > r  = error "row: index out of bounds"
    | otherwise       = V.slice ((i - 1) * c) c v

-- Extract a single column (1-based)
column :: Int -> Matrix a -> Vector a
column j (Matrix r c v)
    | j < 1 || j > c  = error "column: index out of bounds"
    | otherwise       = V.generate r (\i -> v V.! (i * c + (j - 1)))

-- Transpose the matrix
transpose :: Matrix a -> Matrix a
transpose (Matrix r c v) =
    Matrix c r $ V.generate (r * c) $ \idx ->
        let i = idx `div` r  -- new column index
            j = idx `mod` r  -- new row index
        in v V.! (j * c + i)
