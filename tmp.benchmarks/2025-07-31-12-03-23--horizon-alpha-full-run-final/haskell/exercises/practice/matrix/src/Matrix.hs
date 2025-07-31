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

-- Row-major dense matrix with boxed elements
data Matrix a = Matrix
  { mRows :: !Int
  , mCols :: !Int
  , mData :: Vector a
  } deriving (Eq, Show)

-- Number of columns
cols :: Matrix a -> Int
cols = mCols

-- Return the specified column (0-based index)
column :: Int -> Matrix a -> Vector a
column j (Matrix r c v) =
  V.generate r (\i -> v V.! (i * c + j))

-- Flatten the matrix to a single vector in row-major order
flatten :: Matrix a -> Vector a
flatten = mData

-- Construct a matrix from list of rows (must be rectangular)
fromList :: [[a]] -> Matrix a
fromList [] = Matrix 0 0 V.empty
fromList xss =
  let r = length xss
      c = length (head xss)
      v = V.fromList (concat xss)
  in Matrix r c v

-- Parse matrix from string of rows separated by newlines, columns by spaces
fromString :: Read a => String -> Matrix a
fromString s =
  let rowsList = lines s
      parseRow rowStr = map read (words rowStr)
  in fromList (map parseRow rowsList)

-- Reshape matrix to new (rows, cols). No validation on size mismatch.
reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r', c') (Matrix _ _ v) = Matrix r' c' v

-- Return the specified row (0-based index)
row :: Int -> Matrix a -> Vector a
row i (Matrix _ c v) =
  V.slice (i * c) c v

-- Number of rows
rows :: Matrix a -> Int
rows = mRows

-- Shape as (rows, cols)
shape :: Matrix a -> (Int, Int)
shape m = (rows m, cols m)

-- Transpose the matrix
transpose :: Matrix a -> Matrix a
transpose (Matrix r c v)
  | r == 0 || c == 0 = Matrix c r V.empty
  | otherwise =
      let v' = V.generate (r * c) $ \k ->
                 let i = k `div` r
                     j = k `mod` r
                 in v V.! (j * c + i)
      in Matrix c r v'
