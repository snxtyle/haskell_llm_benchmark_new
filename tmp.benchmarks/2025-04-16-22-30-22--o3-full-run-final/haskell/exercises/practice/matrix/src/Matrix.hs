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
import           Data.Vector (Vector)

-- | A simple row‑major matrix backed by boxed vectors.
newtype Matrix a = Matrix (Vector (Vector a))
    deriving (Eq, Show)

-- | Number of rows in the matrix.
rows :: Matrix a -> Int
rows (Matrix v)
  | V.null v  = 0
  | otherwise = V.length v

-- | Number of columns in the matrix.
--   Assumes the matrix is rectangular (all rows have the same length).
cols :: Matrix a -> Int
cols (Matrix v)
  | V.null v  = 0
  | otherwise = V.length (V.head v)

-- | Return the shape of the matrix as (rows, columns).
shape :: Matrix a -> (Int, Int)
shape m = (rows m, cols m)

-- | Extract the i‑th row (1‑based).
row :: Int -> Matrix a -> Vector a
row i (Matrix v) = v V.! (i - 1)

-- | Extract the j‑th column (1‑based).
column :: Int -> Matrix a -> Vector a
column j (Matrix v) = V.map (V.! (j - 1)) v

-- | Flatten the matrix into a single vector in row‑major order.
flatten :: Matrix a -> Vector a
flatten (Matrix v) = V.concat (V.toList v)

-- | Build a matrix from a list of list of elements (row‑major).
fromList :: [[a]] -> Matrix a
fromList xss = Matrix $ V.fromList (map V.fromList xss)

-- | Parse a matrix from its textual representation.
--   Each line is a row, elements are separated by whitespace
--   and parsed using 'Read'.
fromString :: Read a => String -> Matrix a
fromString = fromList . map (map read . words) . lines

-- | Reshape a matrix to the given dimensions (rows, columns).
--   The total number of elements must remain unchanged;
--   otherwise this function will raise a runtime error.
reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r, c) m = Matrix $
    V.generate r $ \i -> V.slice (i * c) c flat
  where
    flat = flatten m

-- | Transpose the matrix.
transpose :: Matrix a -> Matrix a
transpose (Matrix v)
  | V.null v  = Matrix V.empty
  | otherwise = Matrix $ V.generate nCols $ \j ->
        V.generate nRows $ \i -> (v V.! i) V.! j
  where
    nRows = V.length v
    nCols = V.length (V.head v)
