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

data Matrix a = Matrix (Vector (Vector a)) deriving (Eq, Show)

cols :: Matrix a -> Int
cols (Matrix m) = if V.null m then 0 else V.length (V.head m)

column :: Int -> Matrix a -> Vector a
column x (Matrix m) = V.map (V.! x) m

flatten :: Matrix a -> Vector a
flatten (Matrix m) = V.concat $ V.toList m

fromList :: [[a]] -> Matrix a
fromList xss = Matrix $ V.fromList $ map V.fromList xss

fromString :: Read a => String -> Matrix a
fromString xs = fromList $ map (map read . words) $ lines xs

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r, c) m = fromList $ splitEvery c $ V.toList $ flatten m
  where
    splitEvery _ [] = []
    splitEvery n list = take n list : splitEvery n (drop n list)

row :: Int -> Matrix a -> Vector a
row x (Matrix m) = m V.! x

rows :: Matrix a -> Int
rows (Matrix m) = V.length m

shape :: Matrix a -> (Int, Int)
shape m = (rows m, cols m)

transpose :: Matrix a -> Matrix a
transpose m = Matrix $ V.map (\i -> column i m) $ V.fromList [0..(cols m - 1)]
