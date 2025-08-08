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

newtype Matrix a = Matrix (Vector (Vector a)) deriving (Eq, Show)

rows :: Matrix a -> Int
rows (Matrix vv) = V.length vv

cols :: Matrix a -> Int
cols (Matrix vv)
  | V.null vv = 0
  | otherwise = V.length (V.head vv)

shape :: Matrix a -> (Int, Int)
shape m = (rows m, cols m)

row :: Int -> Matrix a -> Vector a
row i (Matrix vv) = vv V.! (i - 1)

column :: Int -> Matrix a -> Vector a
column j (Matrix vv) = V.map (V.! (j - 1)) vv

flatten :: Matrix a -> Vector a
flatten (Matrix vv) = V.concat (V.toList vv)

fromList :: [[a]] -> Matrix a
fromList xss = Matrix (V.fromList (map V.fromList xss))

fromString :: Read a => String -> Matrix a
fromString = fromList . map (map read . words) . lines

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r, c) m =
  let flat = flatten m
  in Matrix $ V.generate r (\i -> V.slice (i * c) c flat)

transpose :: Matrix a -> Matrix a
transpose (Matrix vv)
  | V.null vv = Matrix V.empty
  | otherwise =
      let c = V.length (V.head vv)
      in Matrix $ V.generate c (\j -> V.map (V.! j) vv)
