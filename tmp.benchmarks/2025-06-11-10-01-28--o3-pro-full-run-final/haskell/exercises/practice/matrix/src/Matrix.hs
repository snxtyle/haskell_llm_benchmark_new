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

import           Data.Vector (Vector)
import qualified Data.Vector as V

-- | A simple row-major matrix backed by a Vector of row Vectors.
newtype Matrix a = Matrix { unMatrix :: Vector (Vector a) }
    deriving (Eq, Show)

-- | Number of rows in the matrix.
rows :: Matrix a -> Int
rows (Matrix v) = V.length v

-- | Number of columns in the matrix.
cols :: Matrix a -> Int
cols (Matrix v)
  | V.null v  = 0
  | otherwise = V.length (v V.! 0)

-- | Retrieve a single row (1-based index).
row :: Int -> Matrix a -> Vector a
row i m@(Matrix v)
  | i < 1 || i > rows m = error "row: index out of bounds"
  | otherwise           = v V.! (i - 1)

-- | Retrieve a single column (1-based index).
column :: Int -> Matrix a -> Vector a
column j m@(Matrix v)
  | rows m == 0                  = V.empty
  | j < 1 || j > cols m = error "column: index out of bounds"
  | otherwise                    = V.map (\r -> r V.! (j - 1)) v

-- | Flatten the matrix into a single vector in row-major order.
flatten :: Matrix a -> Vector a
flatten (Matrix v) = V.concat (V.toList v)

-- | Build a matrix from a list of lists.
fromList :: [[a]] -> Matrix a
fromList = Matrix . V.fromList . map V.fromList

-- | Parse a whitespace-separated string into a matrix.
fromString :: Read a => String -> Matrix a
fromString = fromList . map (map read . words) . lines

-- | Shape of the matrix expressed as (rows, columns).
shape :: Matrix a -> (Int, Int)
shape m = (rows m, cols m)

-- | Transpose the matrix.
transpose :: Matrix a -> Matrix a
transpose (Matrix v)
  | V.null v  = Matrix V.empty
  | otherwise =
      let r = V.length v
          c = V.length (v V.! 0)
          newRows = V.generate c $ \j ->
                      V.generate r $ \i -> (v V.! i) V.! j
      in Matrix newRows

-- | Reshape the matrix to the given (rows, columns).
--   Fails if the element count does not match.
reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r', c') m
  | r' * c' /= total = error "reshape: dimensions mismatch"
  | otherwise        =
      let flat = flatten m
          newRows = V.generate r' $ \i ->
                      V.slice (i * c') c' flat
      in Matrix newRows
  where
    total = V.length (flatten m)
