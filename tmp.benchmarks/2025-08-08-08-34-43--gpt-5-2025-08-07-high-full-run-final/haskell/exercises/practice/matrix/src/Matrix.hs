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

-- Row-major matrix representation:
-- mData holds elements in row-major order, length = mRows * mCols
data Matrix a = Matrix
  { mRows :: !Int
  , mCols :: !Int
  , mData :: !(Vector a)
  } deriving (Eq, Show)

rows :: Matrix a -> Int
rows = mRows

cols :: Matrix a -> Int
cols = mCols

shape :: Matrix a -> (Int, Int)
shape m = (mRows m, mCols m)

flatten :: Matrix a -> Vector a
flatten = mData

fromList :: [[a]] -> Matrix a
fromList [] = Matrix 0 0 V.empty
fromList xss@(xs : _) =
  let r = length xss
      c = length xs
      v = V.fromList (concat xss)
  in Matrix r c v

fromString :: Read a => String -> Matrix a
fromString = fromList . map (map read . words) . lines

-- 1-based indexing
row :: Int -> Matrix a -> Vector a
row i (Matrix _ c v) =
  let i0 = i - 1
  in V.slice (i0 * c) c v

-- 1-based indexing
column :: Int -> Matrix a -> Vector a
column j (Matrix r c v) =
  let j0 = j - 1
  in V.generate r $ \i -> v V.! (i * c + j0)

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r, c) (Matrix _ _ v) = Matrix r c v

transpose :: Matrix a -> Matrix a
transpose (Matrix r c v) =
  let v' = V.generate (r * c) $ \idx ->
             let ri' = idx `div` r   -- new row index (0 .. c-1)
                 ci' = idx `mod` r   -- new col index (0 .. r-1)
                 -- original indices are swapped
                 idxOld = ci' * c + ri'
             in v V.! idxOld
  in Matrix c r v'
