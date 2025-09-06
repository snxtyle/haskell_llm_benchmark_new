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

newtype Matrix a = Matrix { getVectorMatrix :: Vector (Vector a) } deriving (Eq, Show)

cols :: Matrix a -> Int
cols (Matrix v) = if V.null v then 0 else V.length (V.head v)

column :: Int -> Matrix a -> Vector a
column n (Matrix v) = V.map (V.! n) v

flatten :: Matrix a -> Vector a
flatten (Matrix v) = V.concat (V.toList v)

fromList :: [[a]] -> Matrix a
fromList xss = Matrix $ V.fromList $ map V.fromList xss

fromString :: Read a => String -> Matrix a
fromString = fromList . map (map read . words) . lines

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r, c) (Matrix v) = 
    let flat = V.concat (V.toList v)
        total = r * c
    in if V.length flat /= total
       then error "Cannot reshape to given dimensions"
       else Matrix $ V.generate r (\i -> V.slice (i * c) c flat)

row :: Int -> Matrix a -> Vector a
row n (Matrix v) = v V.! n

rows :: Matrix a -> Int
rows (Matrix v) = V.length v

shape :: Matrix a -> (Int, Int)
shape matrix = (rows matrix, cols matrix)

transpose :: Matrix a -> Matrix a
transpose (Matrix v) = 
    let lists = map V.toList (V.toList v)
        transposedLists = L.transpose lists
    in Matrix $ V.fromList $ map V.fromList transposedLists
