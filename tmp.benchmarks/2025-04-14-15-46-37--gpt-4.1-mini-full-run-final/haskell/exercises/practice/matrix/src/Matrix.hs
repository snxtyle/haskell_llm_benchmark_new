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
import qualified Data.List as L

-- Define Matrix as a wrapper around Vector (Vector a)
newtype Matrix a = Matrix (Vector (Vector a))
    deriving (Eq, Show)

cols :: Matrix a -> Int
cols (Matrix v)
    | V.null v = 0
    | otherwise = V.length (V.head v)

column :: Int -> Matrix a -> Vector a
column i (Matrix v) = V.map (V.! i) v

flatten :: Matrix a -> Vector a
flatten (Matrix v) = V.concat (V.toList v)

fromList :: [[a]] -> Matrix a
fromList xss = Matrix $ V.fromList (map V.fromList xss)

fromString :: Read a => String -> Matrix a
fromString xs = fromList $ map (map read . words) (lines xs)

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r, c) m =
    let flat = flatten m
        newRows = [V.slice (i * c) c flat | i <- [0 .. r - 1]]
    in Matrix (V.fromList newRows)

row :: Int -> Matrix a -> Vector a
row i (Matrix v) = v V.! i

rows :: Matrix a -> Int
rows (Matrix v) = V.length v

shape :: Matrix a -> (Int, Int)
shape m = (rows m, cols m)

transpose :: Matrix a -> Matrix a
transpose (Matrix v) =
    let rowsList = V.toList v
        transposedList = L.transpose (map V.toList rowsList)
    in Matrix $ V.fromList (map V.fromList transposedList)
