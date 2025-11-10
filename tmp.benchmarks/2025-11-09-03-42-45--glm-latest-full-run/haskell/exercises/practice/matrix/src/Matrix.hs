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
import Data.Vector (Vector)

-- | A matrix is a vector of row-vectors.
newtype Matrix a = Matrix (Vector (Vector a))
    deriving (Eq, Show)

-- | Get the number of columns in a matrix.
cols :: Matrix a -> Int
cols = snd . shape

-- | Get a specific column by its 0-based index.
column :: Int -> Matrix a -> Vector a
column i (Matrix v) = V.map (V.! i) v

-- | Flatten a matrix into a single vector in row-major order.
flatten :: Matrix a -> Vector a
flatten (Matrix v) = V.concat (V.toList v)

-- | Create a matrix from a list of lists, where each inner list is a row.
fromList :: [[a]] -> Matrix a
fromList xss = Matrix $ V.fromList (map V.fromList xss)

-- | Create a matrix from a string. Each line is a row, and words are separated by spaces.
fromString :: Read a => String -> Matrix a
fromString = fromList . map (map read . words) . lines

-- | Reshape a matrix to new dimensions. Fails if the total number of elements doesn't match.
reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r, c) m =
    let flat = flatten m
        len = V.length flat
    in if len /= r * c
       then error "Cannot reshape: new dimensions do not match element count"
       else Matrix $ V.unfoldr splitChunk flat
  where
    splitChunk v
        | V.null v = Nothing
        | otherwise = let (chunk, rest) = V.splitAt c v in Just (chunk, rest)

-- | Get a specific row by its 0-based index.
row :: Int -> Matrix a -> Vector a
row i (Matrix v) = v V.! i

-- | Get the number of rows in a matrix.
rows :: Matrix a -> Int
rows = fst . shape

-- | Get the shape of the matrix as a (rows, cols) tuple.
shape :: Matrix a -> (Int, Int)
shape (Matrix v)
    | V.null v = (0, 0)
    | otherwise = (V.length v, V.length (v V.! 0))

-- | Transpose a matrix (swap rows and columns).
transpose :: Matrix a -> Matrix a
transpose m = fromList $ map V.toList [column i m | i <- [0..cols m - 1]]
