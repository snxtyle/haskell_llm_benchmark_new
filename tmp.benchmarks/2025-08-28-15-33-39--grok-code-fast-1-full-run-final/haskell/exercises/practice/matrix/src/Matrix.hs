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

newtype Matrix a = Matrix { getMatrix :: Vector (Vector a) } deriving (Eq, Show)

cols :: Matrix a -> Int
cols matrix = if rows matrix == 0 then 0 else V.length (V.head (getMatrix matrix))

column :: Int -> Matrix a -> Vector a
column j matrix = V.map (V.! (j - 1)) (getMatrix matrix)

flatten :: Matrix a -> Vector a
flatten matrix = V.concat (V.toList (getMatrix matrix))

fromList :: [[a]] -> Matrix a
fromList xss = Matrix $ V.fromList (map V.fromList xss)

fromString :: Read a => String -> Matrix a
fromString s = fromList $ map (map read . words) (lines s)

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (newRows, newCols) matrix =
  let flat = flatten matrix
  in Matrix $ V.generate newRows (\i -> V.slice (i * newCols) newCols flat)

row :: Int -> Matrix a -> Vector a
row i matrix = getMatrix matrix V.! (i - 1)

rows :: Matrix a -> Int
rows matrix = V.length (getMatrix matrix)

shape :: Matrix a -> (Int, Int)
shape matrix = (rows matrix, cols matrix)

transpose :: Matrix a -> Matrix a
transpose matrix =
  let r = rows matrix
      c = cols matrix
  in Matrix $ V.generate c (\i0 -> V.generate r (\j0 -> row (j0 + 1) matrix V.! i0))
