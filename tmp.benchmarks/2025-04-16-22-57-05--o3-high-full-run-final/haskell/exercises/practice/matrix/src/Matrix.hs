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

-- | A simple row‑major matrix backed by boxed vectors.
newtype Matrix a = Matrix { unMatrix :: Vector (Vector a) }
    deriving (Eq, Show)

-- | Number of columns in the matrix.
cols :: Matrix a -> Int
cols (Matrix v)
  | V.null v  = 0
  | otherwise = V.length (V.head v)

-- | Extract the n‑th column (1‑based).
column :: Int -> Matrix a -> Vector a
column n (Matrix v) = V.map (V.! (n - 1)) v

-- | Flatten the matrix into a single vector in row‑major order.
flatten :: Matrix a -> Vector a
flatten (Matrix v) = V.foldl' (V.++) V.empty v

-- | Build a matrix from a list of lists (row‑major order).
fromList :: [[a]] -> Matrix a
fromList = Matrix . V.fromList . map V.fromList

-- | Parse a textual representation of a matrix.
--   Rows are separated by newlines and elements by white‑space.
fromString :: Read a => String -> Matrix a
fromString = fromList . map (map read . words) . lines

-- | Reshape a matrix.  The total number of elements must stay the same.
reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r', c') m
  | r' * c' /= total = error "reshape: shape mismatch"
  | otherwise        = Matrix $
        V.generate r' $ \i -> V.slice (i * c') c' flat
  where
    flat  = flatten m
    total = V.length flat

-- | Extract the n‑th row (1‑based).
row :: Int -> Matrix a -> Vector a
row n (Matrix v) = v V.! (n - 1)

-- | Number of rows in the matrix.
rows :: Matrix a -> Int
rows (Matrix v) = V.length v

-- | Matrix dimensions as (rows, columns).
shape :: Matrix a -> (Int, Int)
shape m = (rows m, cols m)

-- | Transpose a matrix.
transpose :: Matrix a -> Matrix a
transpose m@(Matrix v)
  | rows m == 0 || cols m == 0 = m
  | otherwise =
      Matrix $ V.generate (cols m) $ \c ->
          V.generate (rows m) $ \r ->
              (v V.! r) V.! c
